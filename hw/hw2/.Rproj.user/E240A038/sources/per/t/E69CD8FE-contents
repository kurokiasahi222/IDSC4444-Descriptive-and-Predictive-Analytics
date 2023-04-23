#Load the package arules in R
#install.packages("arules")
library(arules)

#We cannot load transaction data using the traditional read.csv
temp <- read.csv("coffeeshop.csv", header = FALSE)
temp 

#We need to use a different function
#Load the coffeshop transactions using read.transactions()
#we specify that the format of our data is "basket"
#sep ="," is used to specify how the data are saved in the csv file
#rm.duplicates will remove duplicate items in the same transaction
coffee_data = read.transactions("coffeeshop.csv", format = "basket",
                           sep = ",", rm.duplicates = TRUE)


#To visualize the transactions in the data, use inspect()

View(coffee_data)
inspect(coffee_data)

#To learn the unique items existing in the dataset
itemInfo(coffee_data)

#To learn how many items in each transaction, we can use
size(coffee_data)

#To find the support percentage of each unique item (frequency)
itemFrequency(coffee_data)

#To find the support count, add the option type and set = absolute
itemFrequency(coffee_data, type = "absolute")
?itemFrequency

#To plot the support %
itemFrequencyPlot(coffee_data, ylim = c(0, 1), main = 
                    "Support %", col = "steelblue3")

#we can have the items ordered based on support % 
#(or differently, if you have a large dataset, you can ask to see only the 
#top N items, where N is a number of your choice)
itemFrequencyPlot(coffee_data, ylim = c(0, 1), main = 
                    "Support %", col = "steelblue3", topN = 3)

#You can change the graph to be horizontal
itemFrequencyPlot(coffee_data, main = "Support %", col = 
                    "steelblue3", topN = 5, hor = TRUE, xlim = c(0,1))

#Visualize the entire dataset
#On the horizontal axis, you have individual items; each column tells us in which
#transaction the corresponding item appears; on the vertical axis, you have the transactions
#each row tells us which items are included in the corresponding transaction
#
itemLabels <- c("bagel", "chocolate", "coffee", "cookie", "tea")
image(coffee_data, xlab = itemLabels)
image(coffee_data, xlab = colnames(coffee_data))
coffee_data
colnames(coffee_data)
?image
#subset transactions by item
#the operator %in% will look for transactions that contain the items specified
#if more than 1 item is specified, %in% will look for transactions that have either item

subset(coffee_data, items %in% c("coffee"))
subset(coffee_data, items %in% c("cookie", "tea"))

subtemp <- subset(coffee_data, items %in% c("coffee"))
inspect(subtemp)



#If we want to know transactions that contain ALL the items specified,
#we need to use the operator %ain%, AND
subset(coffee_data, items %ain% c("cookie", "tea"))

#contingency table, use crosstable
#We can compute different measures for pairs of items 
#use support for support percentage
?crossTable
crossTable(coffee_data, sort = TRUE, measure = "support")
#use count for support count
crossTable(coffee_data, sort = TRUE, measure = "count")
#use lift for the lift ratio
crossTable(coffee_data, sort = TRUE, measure = "lift")

#Show all the itemsets above a certain support threshold
?eclat
coffee_itm <- eclat(coffee_data
                    , parameter = list(support = 0.5))

inspect(coffee_itm)

#How to find frequent item-sets and association rules using the Apriori

#mine frequent item-sets
#specify the parameters, such as minsupp and the target of your analysis
#"frequent" means we are interested in the frequent item-sets
?list
coffee_data
?apriori
inspect(coffee_data)
frequent <- apriori(coffee_data, parameter = list(supp = 0.5, target = "frequent"))


#we can get a summary of the frequent item-sets
summary(frequent)

#mine association rules
#specify minsupp and minconfidence
rules <- apriori(coffee_data, parameter = list(supp = 0.5, conf = 0.8, target = "rules"))
inspect(rules)

#we can also specify the min number of items that should be included in the
#association rule
rules2 <- apriori(coffee_data, parameter = list(supp = 0.5, conf = 0.8, target = "rules", minlen = 3))
inspect(rules2)

#Combine inspect with sort
inspect(sort(rules, by = "support"))

#Combine inspect with subset
inspect(subset(rules, lhs %in% "tea"))

inspect(subset(rules, rhs %in% "tea"))

#Binary Matrix Format
#Load the data using the read.csv()
coffee_binary = read.csv("coffeeshop_binary.csv")
#Then transform the data to matrix format
coffee_matrix = as.matrix(coffee_binary)
#Finally, transform the matrix into transactions
coffee_transaction = as(coffee_matrix, "transactions")

#Useful package for association rules reporting and visualization
#install.packages("arulesViz")
library(arulesViz)

#It shows an HTML interactive table
inspectDT(rules)

#We can plot the rules as a graph
set.seed(3)
plot(rules, method = "graph")
plot(rules, method = "graph", edgeCol = "#5E5E5EFF", nodeCol="#56B4E9")
plot(rules, method = "graph", edgeCol = "#E69F00")
plot(rules, method = "graph", edgeCol = "#56B4E9")


#If you have a large dataset and a graph becomes infeasible
#we can plot a scatterplot of the rules, where the color changes based 
#on a chosen measure
plot(rules, measure = c("support", "lift"), shading = "confidence")

