# Association Rule Mining in R
# Load the arules package, if needed
library(arules)
#---------------------------------------------------------------------------------#

# In the first exercise, use the "supermarket.csv" file
# This dataset contains 8 shopping basksets

# P1: Import this dataset as transaction data
# Think about parameters including format, sep, and rm.duplicates
# Write your code below:
?read.transactions
supermarket <- read.transactions("supermarket.csv", format ="basket", 
                                 sep = ",", rm.duplicates = TRUE)
inspect(supermarket)
  
#---------------------------------------------------------------------------------#

# P2: Understand the supermarket data
# Which unique items are there in all shopping baskets?
# Write your code below:
itemInfo(supermarket)

#---------------------------------------------------------------------------------#

# P3: Understand the supermarket data
# How many transactions contain purchases of Butter?
# Write your code below:
butter_data <- subset(supermarket, items %in% c("Butter"))
inspect(butter_data)

#How many transactions contain purchase of Butter and Cheese?
# Write your code below:
butter_cheeze <- subset(supermarket, items %ain% c("Butter", "Cheese"))
inspect(butter_cheeze)
#---------------------------------------------------------------------------------#

# P4: Understand the supermarket data
# Plot the support percentage of each item, for the top 4 items
# Write your code below:
?itemFrequencyPlot
itemFrequencyPlot(supermarket, main = "Support %", 
                  topN = 4)

#---------------------------------------------------------------------------------#
  
# P6: Mine association rules
# Find all association rules with minsupp = 0.375 and minconf = 0.65 and with a min number of items = 2
# Write your code below:
?apriori
rules <- apriori(supermarket, parameter = list(supp = 0.375, conf = 0.65, minlen = 2))


#---------------------------------------------------------------------------------#

# P7: Mine association rules
# Inspect the found rules, in the order of decreasing lift ratio
# Write your code below:
inspect(rules)
inspect(sort(rules, by = "lift"))


#---------------------------------------------------------------------------------#

# In the second exercise, we use the "book.csv" file
# This dataset contains 2000 book purchases in a binary matrix format

# P1: Import this dataset as transaction data
# Think about the three steps of importing
# Write your code below:
?read.transactions
book_data_frame = read.csv("book.csv")
book_matrix <- as.matrix(book_data_frame)
?as
book_transactions <- as(book_matrix, "transactions")
#---------------------------------------------------------------------------------#

# P2: Understand the book data
# Plot the frequency plot, using absolute count
# Which book category sells best? 
# Write your code below:

inspect(head(book_transactions))
summary(book_transactions)
?itemFrequencyPlot
itemFrequencyPlot(book_transactions, ylim = c(0, 1000), type = "absolute", 
                  main = "Top 5 genre ",col= "#053259", topN = 5)
# cooking books sell best
#---------------------------------------------------------------------------------#

# P3: Mine association rules
# Find all association rules with minsupp = 0.1 and minconf = 0.8
# Write your code below:
book_rules <- apriori(book_transactions, parameter = list(supp = 0.1, conf = 0.8))

#---------------------------------------------------------------------------------#

# P4: Understand the rules found
# Inspect the rules, and answer the following questions:
# Which rule has the highest lift? What does it tell us?
# What can be done with this rule, if you were the bookstore manager?
# Write your code below:
inspect(sort(book_rules, by = "lift")) 
# HIGEST LIFT = Italian book -> CookBooks
# People who buy Italian books are 2.32 times more likely to buy cook books 
# than other people


#---------------------------------------------------------------------------------#
#P5: Plot the rules using arulezViz
#Load the package arulesViz, plot a graph of the rules
library(arulesViz)
plot(book_rules, method = "graph")
#Finally, plot a scatterplot, where the shading changes depending on the confidence
plot(book_rules, measure = c("support", "lift"), shading = "confidence")





