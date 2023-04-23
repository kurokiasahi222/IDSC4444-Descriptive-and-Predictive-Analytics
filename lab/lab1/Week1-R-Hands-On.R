# Some basic operations in R
#---------------------------------------------------------------------------------#

# Q1: R as a smart calculator and variable initialization
# Calculate the	average	(mean)	of	12,	13,	14,	15,	16 , 199 and save it in a variable x

#You can store the numbers in a vector using the c() command
num <- c(12,	13,	14,	15,	16 , 199)

#Then, you can compute the mean by taking the sum of x/6
x <- sum(num)/6

#Or use directly the function mean()
x <- mean(num)

x
# 2. Calculate the square root of pi and save it into a variable y
y <- pi^2
y


#---------------------------------------------------------------------------------#
# Q2:Load data from the internal dataset iris. 
#1. Use the data() function.
data(iris)

#2. View the data using View()
View(iris)

#3. Show first 10 rows of the data. Use the head() function
head(iris)

#---------------------------------------------------------------------------------#

# Q3: accessing values in a data frame

# Example: Find the value on row 15, column 4
iris[15,4]

#In the command above, think of the [ ] as representing your data table; 
#the first number you type in, represents a row in the data frame. 
#the second number you type in, after the comma, represents a column in the data frame
#so, in the example above, 15 is the row number, while 4 is the column number.
#you do not need to specify both number. 
#If you need to get only a row, for example,
#you can simply type in the first number, the comma, and leave the other blank.

#1. Find the first Row of Iris:
iris[1, ]

#2. Find the first Column of Iris:
iris[1]

# If you know the name of the desired variable/column, you can directly use the name of the column, rather than the number

#3. Extract all values of variable "Sepal.Length"

iris[,"Sepal.Length"]

#When using column names, you can also use the "$" operator
iris$Sepal.Length

#---------------------------------------------------------------------------------#

# Q4: Import a csv data file with special delimiter

# 1. Download the "coffee.csv" file from Canvas. Import it, and store it as CoffeeData
# Note that the delimiter of this file is not comma, it is semicolon (;)
# We need to specify the delimiter in read.csv() function with the "sep" parameter 
# Fill out the following code, get help from the "Help" tab on your right

CoffeeData <- read.csv("coffee.csv" )

#2. Review the data you loaded by displaying the first 5 rows:
head(CoffeeData, 5)


#---------------------------------------------------------------------------------#

# Q5: Install packages
# 
#1. Install "ggplot2" package on your computer, then load it
# Write your code below to load the library:
install.packages("ggplot2")
library(ggplot2)
#---------------------------------------------------------------------------------#