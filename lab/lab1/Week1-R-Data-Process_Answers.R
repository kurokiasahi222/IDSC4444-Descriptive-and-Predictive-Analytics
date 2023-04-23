# Data cleaning and visualization in R
# We will use Coffee dataset to practice
# Import Coffee data, use your code from last exercise:
CoffeeData <-  read.csv("coffee.csv",sep = ";")
  #---------------------------------------------------------------------------------#
  
  # Q1: Check for missing values
  # Summarize the variables in coffee data, check for potential missing values
  # Write your code below:
  summary(CoffeeData)
# You can also check rows with missing values explicitly using is.na(). You can pass any R object to it.
  is.na(CoffeeData)
  which(is.na(CoffeeData$COGS))
#There is a missing value for COGS, row 10. We replace that with 58
  CoffeeData$COGS[10] <-58
  #---------------------------------------------------------------------------------#
  
  # Q2: Arrange data
  # What is the ProductID of the most profitable coffee?
  # Write your code below:
  #First, we sort the data based on Profit, in decreasing order
CoffeeData_sorted <- CoffeeData[order(CoffeeData$Profit,decreasing = TRUE),]
#then, to visualize the max value, we have different options.
#Option 1: We can look at the head of the sorted dataset, and specify n =1
head(CoffeeData_sorted,n=1)
#Option 2: look at the dataframe, row 1
CoffeeData_sorted[1,]
#Option 3: from the original dataset, take the Profit column CoffeeData$Profit and ask for the row
#that is exactly equal (==) to the maximum of the values in the Profit column
CoffeeData[CoffeeData$Profit==max(CoffeeData$Profit),]
  #---------------------------------------------------------------------------------#
  
  # Q3: Subset data
  # Find the subset of data with Sales larger than 200
  # Write your code below:
  CoffeeData_subset <- subset(CoffeeData, Sales>200)
  #---------------------------------------------------------------------------------#
  
  # Q4: Plot histogram
  # Plot the distribution of COGS, using the original dataset
  # Write your code below:
  library(ggplot2)
  #basic graph
  hist(CoffeeData$COGS,main = "COGS", col = "Blue", xlab = "Cost of Goods", xlim = c(0, 400) )
  
  #ggplot, example 1
  ggplot(data = CoffeeData, aes(x = COGS)) +
  geom_histogram(color = "darkblue" , fill = "blue", alpha = 0.5)  + 
    xlim (0, 400)
  #ggplot, example 2: get a different gradient of color depending on the count
  ggplot(data = CoffeeData, aes(x = COGS, fill = ..count..)) +
    geom_histogram(alpha = 0.5)  + 
    xlim (0, 400) + 
  scale_fill_gradient(low="blue", high="red")
  #--------------------------------------------------------------------------------#
  
  # Q5: Plot boxplot
  # Plot the distribution of profit across different products
  # Write your code below:
  #basic boxplot
  boxplot(Profit~ProductId,CoffeeData, ylim = c(-700, 700))
  
  #example using ggplot
  ggplot(data = CoffeeData, aes(y=Profit, group =ProductId, color = ProductId)) +
     geom_boxplot(notch = TRUE, outlier.colour="red", outlier.shape=8, outlier.size=2)
  
  colnames(CoffeeData)
  #---------------------------------------------------------------------------------#

# Q6: Plot scatterplot
# Plot the relationship between marketing and sales
# Write your code below:
#example using plot
plot(CoffeeData$Marketing, CoffeeData$Sales, xlim=c(0,200), ylim = c(0, 1000), xlab = "Marketing", 
ylab = "Sales")

#example using qplot
qplot(data = CoffeeData, x = CoffeeData$Marketing, y = CoffeeData$Sales, col = CoffeeData$ProductId)
#---------------------------------------------------------------------------------#