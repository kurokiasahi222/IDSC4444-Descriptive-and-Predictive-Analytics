
#Load the package stats
# install.packages("factoextra")
library(stats) 
library(ggplot2)
library(factoextra)

#Load the data from the US_stats file
data_states = read.csv("US_States_2014.csv")

#Explore the data to get a sense of the attributes
head(data_states, n =3)
#get a summary
summary(data_states)

#Objective: create clusters of states, based on the available attributes

#Decide whether there is the need to normalize the data
#Data normalization is almost always necessary; in our case, even if we have all numerical data,
#they take different ranges. You can look at the summary, to get a sense, for example, of the ranges of
#your attributes

#If we conclude that we need to normalize the data, we need to choose which method to use
#and we need to assess which attributes to normalize.
#In our case, the column State contains the name of each State; it is not an attribute we will use
#for clustering. As such, we exclude it from the normalization.

#How to normalize with the min-max method: we can create our own function in R
#We are going to create the function normalize
#The function takes as input a vector (or column) of numbers and performs the min-max normalization

normalize = function(x){
  return ((x - min(x))/(max(x) - min(x)))}

#Example of how the normalize function works. Remember, min-max will transform your values
#to be between 0 and 1
normalize(c(1,2,3,4,5))

#The other method we could use is standardization. We can use the existing package in R

library(standardize)

#Example of how scale works.
#Remember standardization will transform your attributes to have mean 0 and SD 1
scale(c(1,2,3,4,5))

#Let us normalize our data using the min-max function
#First, create a copy of the dataset, so we can keep the original data
data_norm <- data_states

#Next, apply the normalize function to our dataset, using apply()
#We nee to specify which attributes we want to normalize.
#We concluded we want to exclude the State column, so we will use column from 2 to 10
#the option MARGIN specifies whether we want to apply the function by row or by column. 
#The = 2 means by column
#the option FUN is used to specify which function we want to apply to the data

data_norm[, 2:10] <- apply(data_norm[, 2:10], MARGIN = 2, FUN = normalize)


#If we want to use standardization, apply the function scale to the dataset
data_stand <- data_states
data_stand[, 2:10] <- apply(data_stand[, 2:10], MARGIN = 2, FUN = scale)

#------------------------------------------------------------------------#
#HIERARCHICAL CLUSTERING

#EXAMPLE 1
#Let us start implementing hierarchical clustering
#First, compute the distance matrix, using a distance measure of choice between 
#euclidian, maximum and manhattan.
#The distance matrix contains the pairwise distances between points
#Let us use the euclidian distance
#The distance matrix is computed using the normalized data. 
#Let us use the min-max normalization
?dist
distance_matrix_norm = dist(data_norm[, 2:10], method = "euclidean")

#If you want to see the complete distance matrix
View(as.matrix(distance_matrix_norm))

#Next, we implement the hierarchical clustering algorithm using the hclust() function
#We need to input the distance matrix and we need to specify which (dis)similarity measure
#we want to use to measure distance between clusters, among:
#single, complete, average, centroid, ward.D
#if you run help(hclust) you will see all the methods available
?hclust
h1 = hclust(distance_matrix_norm, method = "ward.D")

#To get the dendrogram, we use plot and input the hierarchical results
plot(h1, hang = -1, cex = 0.4)

#if wished, we can change the names of the labels
plot(h1, hang = -1, cex = 0.4, labels = data_states$State)

#Draw clusters on the dendrogram; pick the number of clusters you want: k = 
#the option border is used to pick different colors for the borders of the clusters
rect.hclust(h1, k = 4, border = 2:5)
?rect.hclust

rect.hclust(h1, k = 6, border = 2:7)

#Cut the dendrogram into groups
#The function cutree() can be used to get a vector containing the cluster number of each observation:
hcluster_4 <- cutree(h1, k = 4)

# Number of members/points in each cluster
table(hcluster_4)

#To see which observations belong to the 4 clusters, we can create 
#a new column in our original dataset with the cluster numbers
data_states$hcluster_4 <- hcluster_4

#and then we can look at the summary statistics for the different clusters
#for easy of interpretation, we usually take the summary statistics of the original, non-normalized data
summary(subset(data_states, data_states$hcluster_4 == 1))
summary(subset(data_states, data_states$hcluster_4 == 2))

#to make this process a little more efficient, we can use the package plyr
# install.packages("plyr")
library(plyr)

#we use the function ddply, where we specify the original data we want to use for the summarization
#which variable we want to use to "group" our data
#which function we want to use: in this case, summarize
#which attributes we want to use and which stats we want; in this case we want the mean for Female_MedianAge and
#for Robbery; we can add more attributes if desired
?ddply
ddply(data_states, .(hcluster_4), summarize, F_MedAge=mean(Female_MedianAge), Robbery=mean(Robbery))

#We could also create a table to look at the distribution of the clusters across different
#categorizations of our data-points
#In this dataset, for example, the States are categorized by Regions.
#We can see the distribution of the clusters by Region
table(data_states$Region, data_states$hcluster_4)

#and we could repeat the summarization of some of the attributes by Region 
ddply(data_states, .(Region), summarize, F_MedAge=mean(Female_MedianAge), Robbery=mean(Robbery))

#EXAMPLE 2
#Repeat using a different distance measure for the clusters
#In this case, I am using average linkage

h2 = hclust(distance_matrix_norm, method = "average")
plot(h2, labels = data_states$State)
rect.hclust(h2, k = 4, border = 2:4)
h2_4 <- cutree(h2, k = 4)
table(h2_4)
data_states$h2_4 <- h2_4

#---------------------------------------------------------#
#K-MEANS CLUSTERING
#Next, let us implement k-means, using the function kmeans()
#We need to input normalized data
#and we need to specify how many clusters we want; let us say, 4
#NOTE: remember that k-means selects 4 random points to begin with
#If we want kmeans to run for a number of different random starting point, we can use 
#the option nstart
#In the example below, R will try 15 different sets of random starting points 
#and then select the one with the lowest within cluster variation.

#Since the points are picked at random, every time we run
#the command it may change; 
#if you do not want that, because you may want to be able to replicate 
#the same analysis, use set.seed

set.seed(123)
?kmeans
k1 = kmeans(data_norm[, 2:10], centers = 4, nstart = 15)

#Look at a summary of the result
str(k1)

#check size of each cluster
k1$size

#check cluster centroids
k1$centers

#Add a column to the original data to indicate in which cluster each observation is
data_states$k_clust <- k1$cluster

#Use the original dataset to do summary statistics like we did before
ddply(data_states, .(k_clust), summarize, F_MedAge=mean(Female_MedianAge), Robbery=mean(Robbery))


#CAN WE PLOT THE CLUSTERS?
#Plotting the clusters would be easy if we had 2 or 3 dimensions; in this example, we have 9 dimensions
#What can we do? We can use the package fviz_cluster.
#If the data contains more than 2 dimensions, the package fviz_cluster will perform something
#called principal component analysis (PCA) and plot the data points according to 
#the first two principal components that explain the majority of the variance.
#Look at slides for a more detailed description

#specify the clustering solution, the data used
?fviz_cluster
fviz_cluster(k1, geom = "point", data=data_norm[, 2:10]) + ggtitle("Clusters")

#We could also decide to pick 2 dimensions and plot the clusters for these two
fviz_cluster(k1, geom = "point", data=data_norm[, 2:10], choose.vars = c("Unemployment_Rate", "Robbery")) + 
  ggtitle("Clusters")
## ??????? Is this performing PCA? 

#while we cannot directly interpret the number of the axis
#we can interpret the "direction". Example, by looking at the plot,
#it seems that states in the red cluster tend to have relatively higher values of 
#robbery and higher unemployment rate.
#on the other hand, there are the states in the purple and light-green clusters
#that seem to have relatively low robbery cases, but the unemployment rate
#is distributed across the spectrum.

#We can also run kmeans for different number of k 
#and plot the results in a grid
library(gridExtra)

k2 <- kmeans(data_norm[, 2:10], centers
             = 2, nstart = 15)
k3 <- kmeans(data_norm[, 2:10], centers = 3, nstart = 15)
k4 <- kmeans(data_norm[, 2:10], centers = 4, nstart = 15)
k5 <- kmeans(data_norm[, 2:10], centers = 5, nstart = 15)

p2 <- fviz_cluster(k2, geom = "point", data = data_norm[, 2:10]) + ggtitle("k = 2")
p3 <- fviz_cluster(k3, geom = "point", data = data_norm[, 2:10]) + ggtitle("k = 3")
p4 <- fviz_cluster(k4, geom = "point",  data = data_norm[, 2:10]) + ggtitle("k = 4")
p5 <- fviz_cluster(k5, geom = "point",  data = data_norm[, 2:10]) + ggtitle("k = 5")

grid.arrange(p2, p3, p4, p5, nrow = 2)


#EVALUATING CLUSTERS

#Let us see how we can compute the WSS and create the Elbow Plot
#Create an empty vector to store the results for the WSS
WSS_curve <- c()

#Create a for-loop, that implements the k-means clustering for how many times we would like,
#each time increasing the number of clusters requested
#during each iteration, we also ask R to compute the WSS and to save
#the results into WSS_curve 

#in this example, we run k-means for 10 times (from 1 to 10)
#and with nstart = 5
?kmeans
for (n in 1:10) {
  k = kmeans(data_norm[,2:10], centers = n, nstart = 5)
  wss = k$tot.withinss
  WSS_curve[n] <- wss}

#Plot the final results
plot(1:10, WSS_curve, type = "b", col = "red", ylab = "WSS", xlab = "K", ylim = c(0,18) )


#We can also compute the BSS and plot it together with the WSS
WSS_curve <- c()
BSS_curve <- c()

#In this case, we can have the loop computing also the BSS and save the results
#into BSS_curve
for (n in 1:10) {
  k = kmeans(data_norm[,2:10], centers = n, nstart = 5)
  wss = k$tot.withinss
  bss = k$betweenss
  WSS_curve[n] <- wss
  BSS_curve[n] <- bss}

#Finally, we can plot both in the same graph
plot(1:10, WSS_curve, type = "b", col = "red", ylab = "WSS and BSS", xlab = "K", ylim = c(0,18)  ) 
lines(1:10, BSS_curve,type="o",col="blue")






