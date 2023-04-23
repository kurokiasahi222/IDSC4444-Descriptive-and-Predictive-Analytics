# Some basic operations in R
#---------------------------------------------------------------------------------#

# Q1: R as a smart calculator and variable initialization
# Calculate the following two quantities and store them in variables x and y:

#   1. The	average	(mean)	of	12,	13,	14,	15,	16 , 199 .
x <- mean(c(12,	13,	14,	15,	16 ,199))
x
  #   2. Square root of pi.
y <- sqrt(pi)  
y  
  #---------------------------------------------------------------------------------#
  # Q2:Load data from the internal dataset iris. Use the data() function.
 data("iris") 

  # View the data using View()
  View(iris)

  #Show first 10 rows of the data 
  head(iris,n = 10)
  #---------------------------------------------------------------------------------#
  
  # Q3: accessing values in a data frame
  # Find the value on row 15, column 4
  iris[15,4]
# #First Row
iris[1,]

#First Column
iris[,1]

# Extract all values of variable "Sepal.Length", using the "$" operator

iris[,"Sepal.Length"]
iris$Sepal.Length
#---------------------------------------------------------------------------------#

# Q4: Import a csv data file with special delimiter

# Check the current working directory()
 getwd()

# Download the "coffee.csv" file from Canvas. Import it, and store it as CoffeeData
# Note that the delimiter of this file is not comma, it is semicolon (;)
# We need to specify the delimiter in read.csv() function with the "sep" parameter 
# Fill out the following code, get help from the "Help" tab on your right

CoffeeData <- read.csv(file = "coffee.csv",sep = ";")
head(CoffeeData,n=5)
#---------------------------------------------------------------------------------#

# Q5: Install packages
# Install "ggplot2" package on your computer, then load it
# Write your code below to load the library:

install.packages("ggplot2")
library(ggplot2)

#---------------------------------------------------------------------------------#