
# Association Rule Mining in R
# Load the arules package, if needed
library(arules)
#---------------------------------------------------------------------------------#

# In the first exercise, use the "supermarket.csv" file
# This dataset contains 8 shopping basksets

# P1: Import this dataset as transaction data
# Think about parameters including format, sep, and rm.duplicates
# Write your code below:
supermarket <- 
  
#---------------------------------------------------------------------------------#

# P2: Understand the supermarket data
# Which unique items are there in all shopping baskets?
# Write your code below:

#---------------------------------------------------------------------------------#

# P3: Understand the supermarket data
# How many transactions contain purchases of Butter?
# Write your code below:

#How many transactions contain purchase of Butter and Cheese?
# Write your code below:

#---------------------------------------------------------------------------------#

# P4: Understand the supermarket data
# Plot the support percentage of each item, for the top 4 items
# Write your code below:


#---------------------------------------------------------------------------------#
  
# P6: Mine association rules
# Find all association rules with minsupp = 0.375 and minconf = 0.65 and with a min number of items = 2
# Write your code below:

#---------------------------------------------------------------------------------#

# P7: Mine association rules
# Inspect the found rules, in the order of decreasing lift ratio
# Write your code below:



#---------------------------------------------------------------------------------#

# In the second exercise, we use the "book.csv" file
# This dataset contains 2000 book purchases in a binary matrix format

# P1: Import this dataset as transaction data
# Think about the three steps of importing
# Write your code below:

#---------------------------------------------------------------------------------#

# P2: Understand the book data
# Plot the frequency plot, using absolute count
# Which book category sells best? 
# Write your code below:

#---------------------------------------------------------------------------------#

# P3: Mine association rules
# Find all association rules with minsupp = 0.1 and minconf = 0.8
# Write your code below:

#---------------------------------------------------------------------------------#

# P4: Understand the rules found
# Inspect the rules, and answer the following questions:
# Which rule has the highest lift? What does it tell us?
# What can be done with this rule, if you were the bookstore manager?
# Write your code below:

#---------------------------------------------------------------------------------#
#P5: Plot the rules using arulezViz
#Load the package arulesViz, plot a graph of the rules

#Finally, plot a scatterplot, where the shading changes depending on the confidence
