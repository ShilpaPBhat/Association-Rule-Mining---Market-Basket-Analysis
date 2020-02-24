#' ---	
#' title: "Market Basket Analysis"	
#' output: 	
#'   html_document:	
#'     code_download: yes	
#'     fig_height: 4	
#'     highlight: haddock	
#'     number_sections: no	
#'     theme: default	
#'     toc: yes	
#'     toc_float:	
#'       collapsed: yes	
#'       smooth_scroll: yes	
#' ---	
#' 	
knitr::opts_chunk$set(echo = TRUE)	
path = getwd()	
setwd(path)	
#' 	
#' 	
#' 	
pacman::p_load(arulesViz, arules)	
#' 	
#' # Introduction	
#' 	
#' Market Basket Analysis, also know as affinity analysis or association rule mining, is a data mining technique used mostly in retail to increase sales by focusing on finding purchase patterns by extracting associations from transactional data.	
#' The Apriori algorithm generates association rules. An association rule says that if an product 1 occurs, then product 2 occurs with a certain probability	
#' 	
#' What do my customer buy? What did they buy together? These are the common question or analysis to be made. These analysis revel what people buy together and then can be used to create appropriate promotions or placements of the product in the store. Also to design promotional campaigns.	
#' 	
#' 	
#' # Code	
#' ## Reading Data	
#' * read.transactions(): This is used to read data as the transactions data	
#' * summary(): It gives fiollowing output	
#'   1. Number of unique transactions and unique items	
#'   2. Most frequent items	
#'   3. Number of items per transaction (distribution per transaction)	
#' 	
#' 	
transactions <- read.transactions('transaction_sample.txt', sep = '|', format = 'single', cols = c(1,2))	
summary(transactions)	
inspect(transactions[1:5])	
#' 	
#' 	
#' ## Item Frequency	
#' 	
#' * itemFrequencyPlot(): Plots item frequency (bar plot)	
#' 	
itemFrequencyPlot(transactions, topN = 15, main = 'Item Support Plot', xlab = 'Item')	
#' 	
#' 	
#' **Sampling the data**	
#' 	
#' If the data size is too big to handle we can do a sample selection. But for association rule sample selection one must consider the basic criterion of support of the itemset	
#' Formula for sample selection is, n=(−2)∗log(c)(support∗epsilon2)	
#' But before we go ahead with the sample, we must make sure that the sample is a good replica of the universe. In order to do that we will look at the item frequency plot with an additional paramerter of lift. Since our data is not that big we can consider entire datset.	
#' 	
#' 	
#' ## Apriori Algorithm	
#' **Apriori** is the command for association in R. data should be in transaction class.	
#' Support: This measure gives an idea of how frequent an itemset is in all the transactions	
#'          Support({X} -> {Y}) = Transaction containing both X and Y / Total number of transaction	
#'          Value of support helps us identify the rules worth considering for further analysis	
#' Confidence: This measure defines the likeliness of occurrence of consequent on the cart given that the cart already has the antecedents	
#'             Confidence({X} -> {Y}) = Transaction containing both X and Y / Total number of transaction containing X	
#'             	
#' 	
rules <- apriori(transactions, parameter = list(support = 0.005, confidence = 0.80))	
#' 	
#' 	
#' ## Output Interpretation 	
#' 	
quality(rules) <- round(quality(rules), digits = 4)	
inspect(rules[1:10])	
#' 	
#' 	
#' LHS is antecedent and RHS is consequent	
#' {Pulses,Vermicelli} implies {Edible Oil}	
#' 	
#' support: Of the total no of bills 0.54% bills contain {Pulses, Vermicelli & Edible Oil}	
#' confidence: Of the bills that contain {Pulses & Vermicelli} 81.36% also contains {Edible Oil}	
#' Looks like a high confidence value. There could be something misleading about this high confidence value. Lift is introduced to overcome this challenge.	
#' 	
#' lift: (Transaction containing both X and Y / Total number of transaction containing X) / Fraction of transaction containing Y	
#' 	
#' A value of lift greater than 1 vouches for high association between {Y} and {X}. The larger the lift ratio, the more significant the association.	
#' 	
#' 	
#' ## Subsetting and pruning rules 	
#' 	
#' 	
rules_sorted <- sort(rules, by = 'lift')	
inspect(rules_sorted[1:5])	
subset.matrix <- is.subset(rules_sorted, rules_sorted, sparse = FALSE)	
subset.matrix[lower.tri(subset.matrix, diag = T)] <- NA	
redundant <- colSums(subset.matrix, na.rm = T) >= 1	
rules_pruned <- rules_sorted[!redundant]	
rules_pruned	
inspect(rules_pruned[1:5])	
#' 	
#' 	
#' # Visualization	
#' 	
#' ## Scatter Plot	
#' This visualization method draws a two dimensional scatterplot with different measures of interestingness (parameter "measure") on the axes and a third measure (parameter "shading") is represented by the color of the points.	
#' 	
#' 	
plot(rules_pruned[1:20], measure=c("support","lift"), shading="confidence")	
#' 	
#' 	
#' ## Graph	
#' Represents the rules (or itemsets) as a graph with items as labeled vertices, and rules (or itemsets) represented as vertices connected to items using arrows. For rules, the LHS items are connected with arrows pointing to the vertex representing the rule and the RHS has an	
#' arrow pointing to the item.	
#' 	
#' 	
plot(rules_pruned[1:20], method="graph",control=list(type="items"))	
#' 	
#' 	
#' ## Circular Graph	
#' 	
#' Graph visualization with different layout	
#' 	
#' 	
plot(rules_pruned[1:20],method="graph",control=list(layout = igraph:: in_circle()))	
#' 	
#' 	
#' 	
#' # What rules lead to consequent?	
#' 	
#' This can be done by filtering the rules to see what leads to a particular product	
#' 	
#' 	
filter = 'Spices'	
rules_filtered <- subset(rules_pruned, subset = rhs %in% filter)	
	
inspect(rules_filtered)	
#' 	
