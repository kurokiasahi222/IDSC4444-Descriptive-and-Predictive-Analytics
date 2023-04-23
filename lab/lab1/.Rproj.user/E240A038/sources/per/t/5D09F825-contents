#This the R script for the R Tutorial
# Comments are preceded by the #. Comments are skipped and not executed

#Basic use of R:calculator
1+1
2+3*4
3^2
sqrt(16)
pi

#Variables: "placeholders" that contain the values we are working with
#To define a variable we use the <- operator 
x <- 1
y <- 2
x + y 
z <- x + y

#R is case sensitive
X + y
#Variables names can include periods
this.year <- 2020
this.year 

 #We will be using datasets to work in R
#Loading data
#Internal data: datasets already existing in R (made available by other users)
#To see the existing datasets in R, you can use the command below
data()

#Let us load the internal dataset called swiss
data("swiss")

#We can use the command head() to visualize the first rows of the datasets
#The default is 6 rows; if you want a different number, you need to specify it
head(swiss)
head(swiss, n=3)
?swiss



#We will work with external datasets
#Loading external data
#Default: read.csv() assumes data use comma as delimiter; first row are columns names
#Give a name to your dataset; in the example below, the name is mydata
#Then assign to the name the function read.csv and specify inside the parenthesis
#the path to the data. The path is the location in your laptop, where you saved the data
mydata <- read.csv("C:/Users/zihong/Documents/..")
mydata <- read.csv()

#R uses a working directory: think of a folder saved on your laptop that R
#uses automatically to search for files and to save files
#How to check your current Working Directory
getwd()

#You can change your work directory to be a folder of your choice
#set Working Directory
setwd("E:/Dropbox/Teaching/IDSC 4444 Zihong/Lectures/Lec2")

#after setting the working directory, you do not need to specify the path of your files
#R will automatically go to the folder that you set to be your working directory
iris_data <- read.csv("iris.csv")

#Alternative way to look/load files
read.csv(file.choose())

#How to install packages and load them
#You can install packages from the utility panel
#After the packages are installed, you need to load them before they can be used
library(Matrix)
library(arules)
library(ggplot2)

#PAUSE THE VIDEO AND TRY TO COMPLETE THE HANDS-ON PRACTICE "Week1-R-Hands-On.R"
#SOLUTIONS ARE ALREADY AVAILABLE. TRY TO COMPLETE IT BY YOURSELF FIRST.

#R for data cleaning
#Load the data_cleaning.csv, call it mydata
mydata<- read.csv("data_cleaning.csv")
mydata

#Use the summary() function
summary(mydata)

#Specify variables to summarize
summary(mydata[,3:4])
summary(mydata[,'Age'])
age <- mydata[,'Age']

#Correct missing values
#Identify row with missing value
#the command below will show a True or False for each row-column in your datasets
#if the data is present, it will say False. It the data is missing, it will say True
#this is only helpful if you have a small dataset
is.na(mydata)

#if you have a large dataset, you should first identify which column has missing data (for example, look at summary)
#then you use the following command, where you specify which column to look at
which(is.na(mydata$Age))

#Correct the missing value: once you now which column/row has missing data, you can correct that
mydata$Age[5] <- 33
mydata

#We can sort our data using the order() function
#First, see unsorted data
mydata$Age

#Then, see what the order function does. It creates an index for each value, reprenseting the order of that value in the column.
#By default, the order is increasing. So the index 1 will be assigned to the smallest value.
order(mydata$Age)

#Sort data by age, in decreasing order and store the changed datasets into a new one
mydata_sorted <- mydata[order(mydata$Age, decreasing = TRUE),]
mydata_sorted

#Subset data: use subset() function
#Example 1: subset all the observations that are females
#In the subset function, you need to specify the dataset you want to use and 
#the condition you want to use to subset
#the operator == means exactly equal to and it is used in equality comparisons
mydata_female <- subset(mydata, Gender  =="F")
mydata_female

#Example 2: subset on specified columns. Use the option select = 
#then specify the columns' numbers you want to include.
#1:2 means include columns 1 to 3 (included)
mydata_names <- subset(mydata, select = 1:3)
mydata_names

#Create new variable
#Let us add a new variable, height
height <- c(160, 187, 175, 165, 180)
mydata
mydata$Height <- height
mydata

#How to join two datasets: use function merge()
#first, we create the second dataset; load the data data_cleaning2.csv
mydata2 <- read.csv("data_cleaning2.csv")

#then, we create the merged dataset, using the function merge()
#in the function, we specify the first dataset, the second dataset and the column
#we want to use for the mergin. The column must be common to both datasets.
mydata
mydata2
mydata_merge <- merge(mydata, mydata2, by = "ID")
mydata_merge

#clear workspace

#R for visualization
#Load the internal dataset WorldPhones
data("WorldPhones") 
head(WorldPhones)

#Get only the data for 1951
phones_51 <- WorldPhones[1,]

#Create basic barplot
barplot(phones_51)

#Add/change few options to the barplot
#the option col = lets you specify the color
#the option main = lets you change the title of the plot
#the option ylim = c() lets you specify the initial and last value for the Y axis
barplot(phones_51, col = "blue", main = "#Phones in 1951", ylim = c(0, 50000) )

#Load iris data
iris <- read.csv("iris.csv")
head(iris)

#Create an histogram for the distribution of Sepal Width 
#In the hist() command, first specify which column you want to use
#The you can add options like above. 
#the xlim = c () lets you specify the initial and final value of the X axis
#the xlab lets you specify a label for the X axis
hist(iris$sepal_width, main = "Distribution of Sepal Width", col = "red", 
     ylim = c(0,40), xlim = c(2, 4.5), xlab ="Sepal Weight")

#Create boxplot for Sepal width, by classes (or species)
boxplot(iris$sepal_width ~ iris$class, data = iris, ylim = c(1, 5), range = 0, xlab = "Class", ylab = "Sepal Width", main = "Boxplot of Sepal Width by Class")

#If you want to know the statistics related to the boxplot
#first, save the boxplot as bp
bp <-boxplot(iris$sepal_width ~ iris$class, data = iris, ylim = c(1, 5), range = 0, xlab = "Class", ylab = "Sepal Width", main = "Boxplot of Sepal Width by Class")
#then, run the command below. It will compute the statistics for the boxplot
bp$stats
#finally, add the statistics to the graph
text(x = col(bp$stats) - .5, y = bp$stats, labels = bp$stats)

#Plot relationship between Sepal Length and Sepal Width

plot(iris$sepal_length, iris$sepal_width, xlab = "Sepal Length", 
     ylab = "Sepal Width", ylim = c(0, 5), col = "dark green", main = "Relationship between Sepal Length and Sepal Width")

#ggplot
library(ggplot2)

#To make things easier, I will lave the column sepal_length into its own vector
#and then I will use this vector to make plots
head(iris)
Sepal.Length <- iris$sepal_length
Sepal.Width <- iris$sepal_width

#QPLOT is an easy way to create "quick plots". qplot will try to guess what type of 
#plot you want based on the input

#If you only input one variable, x, qplot will do a histogram 
#Histogram of Sepal.Lenght
#specify in the option data = which dataset you want to use
#x = specifies which column 
qplot(data = iris, x = Sepal.Length, main = "Distribution for Sepal Length", xlab="Sepal Length", ylab = "Count")

#Add a few options
#col = I() changes the color of the lines of the borders of the bars
#fill = I() changes the fill of the bars
qplot(data = iris, x = Sepal.Length, col = I("orange"), fill = I("red"), main = "Distribution for Sepal Length", xlab="Sepal Length", ylab = "Count")

#If you input two variables, qplot will plot a scatterplot
#qplot scatterplot of Sepal.Lenght and Sepal.Width
qplot(data = iris, x = Sepal.Length, y=Sepal.Width, ylim = c(0,5), main = "Relationship between Sepal Length and Width")

#qplot scatterplot of Sepal.Lenght and Sepal.Width, by Species
#the option col = can also be used to assign a different color to different groups in your data.
#for example, we want a different color for each class (or species) of iris
qplot(data = iris, x = Sepal.Length, y=Sepal.Width, col=class, ylim = c(0,5), main = "Relationship between Sepal Length and Width")

#To make professional, customazible graphs, we use ggplot

#ggplot the histogram as before, by class
#specify which data to use, and the aesthetics, aes()
#the geom_ is used to specify the type of plot we want and additional options
#for example, geom_histogram will create an histogram
#the option alpha = specifies the transparency of the colors; the lower the number, the more transparent the colors

ggplot(data= iris, aes(x = Sepal.Length, fill = class)) + 
  geom_histogram(alpha=0.3) +
  ggtitle("Distribution of Sepal Lenght By Class") +
  labs(x = "Sepal Length", y = "Count")


#ggplot for boxplots
#
ggplot(data = iris, aes(x=class, y=sepal_width, color=class)) +
  geom_boxplot(notch = FALSE, outlier.colour="black", outlier.shape=8, outlier.size=3) +
  ggtitle("Boxplot of Sepal Width, by Classes") +
  labs(x = "Class", y = "Sepal Width")

#ggplot for scatterplots
ggplot(data = iris, aes(x=Sepal.Length, y=Sepal.Width, color=class, shape = class)) +
  geom_point(size=5) +
  ggtitle("Relationship between Sepal Length and Sepal Width, separated by Class") +
  labs(x = "Sepal Length", y = "Sepal Width")

#add linear regression lines to the scatterplot
ggplot(data = iris, aes(x=Sepal.Length, y=Sepal.Width, color=class, shape = class)) +
  geom_point(size=2) +
  ggtitle("Relationship between Sepal Length and Sepal Width, separated by Class") +
  labs(x = "Sepal Length", y = "Sepal Width") +
  geom_smooth(method=lm)
  

#Additional examples
#Example 1
#Here we create a histogram and on top of it, a density plot.
#we have both geom_histogram for the histogram's features
#and we also have geom_density for the density plot's features
ggplot(data = iris, aes(x = Sepal.Length)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="light green") +
  labs(x="Sepal Length", y = "Density")+
  ggtitle("Distribution of Sepal Length with Density Plot")

#Example 2
#create a histogram, separated by class and add a density plot for each class.

ggplot(data= iris, aes(x=Sepal.Length, color=class, fill=class)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6, linetype = "dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(x="Sepal Length", y = "Density")+
  ggtitle("Sepal Length histogram plot")




