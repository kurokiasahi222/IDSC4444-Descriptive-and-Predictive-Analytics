#----[Reading the data]
#load the packages
library(stats)
library(factoextra)

#Read the data from constituents-financials_csv.csv
data <- read.csv("constituents-financials_csv.csv")
#Summarize the dataset to get a feel of the data.
summary(data)

#----[Normalization]
#Choose the variables you should normalize, store them in a vector called var_names.
var_names <- c("Price","Dividend.Yield","Earnings.Share","X52.Week.Low","X52.Week.High","Market.Cap","EBITDA","Price.Sales")

##Now normalize the above variables using standardization 
# Use the scale function to standardize your data
library(standardize)

#create a copy of the data
data_scale <- data
data_scale2 <- data

#apply the standardization to the attributes you included in the var_names
# margin = 2: apply to column
data_scale[, 4: 11] <- apply(data_scale[, 4:11], MARGIN = 2, FUN =scale)
data_scale2[, var_names] <- apply(data_scale2[, var_names], MARGIN = 2, FUN = scale)

#Check if all the attributes you standardized have a mean 0
summary(data_scale)
summary(data_scale2)

#----[Hierarchical Clustering]
# Create a dataframe data_IT with only companies in the Information Technology sector.
?subset
unique(data_scale2$Sector)
data.IT <- subset(data_scale2, data_scale2$Sector == "Information Technology")
head(data.IT)

#Rename the rows to company symbol, this should help you identify the distance matrix cells
rownames(data.IT) <- data.IT$Symbol

#Create a distance matrix for the newly filtered dataset using the manhattan measure. 
#What variables should you use?
dist_matrix = dist(data.IT[, var_names], method = "manhattan")


#View the matrix
View(as.matrix(dist_matrix))

# Use the hclust() to generate hierarchical clusters from the d_mat distance matrix
#use the ward method
h1 <- hclust(dist_matrix, method = "ward.D")


#Plot the dendogram from the clusters
plot(h1, hang = -1, cex = 0.4)

#Cut the tree into 5 clusters and plot these 
rect.hclust(h1, k = 5, border = 1:5)

#Assign each company a cluster number and find out which cluster is ORCL in?
h_cluster5 <- cutree(h1, 5)
h_cluster5
table(h_cluster5)
###ORCL = cluster 3


  
#add a column to your dataset with the cluster's numbers
data.IT$h_cluster5 <- h_cluster5
data.IT["ORCL", ]$h_cluster5
data.IT$h_cluster5 == data_IT$clusters
all.equal(data.IT$h_cluster5 , data_IT$clusters)

#Summarize cluster 1 and 4 to get an idea of how they differ from each other
cluster1 <- subset(data.IT, data.IT$h_cluster5 == 1)
cluster4 <- subset(data.IT, data.IT$h_cluster5 == 4)
summary(cluster1)
summary(cluster4)

#----[K-Means]--
# Implement k-means clustering using the standardized data for IT companies as above
#pick a number of centers
set.seed(546)
?kmeans
k1 <- kmeans(data.IT[, var_names], centers = 5)

#what are the sizes of the clusters?
str(k1)
k1$size

# Create the WSS curve by running kmeans for different number of clusters
wss_curve <- c()
for(i in 1:10){
  k <- kmeans(data.IT[, var_names], centers = i)
  wss <- k$tot.withinss
  wss_curve[i] <- wss
}
?plot
plot(1:10, wss_curve,type ="o", col = "#24477f", xlab = "Number of cluster", main = "wss")


#After evaluating the Elbow plot, rerun kmeans with what you think is the most appropriate number of clusters. 
k4 <- kmeans(data.IT[, var_names], centers = 4)

#How many points in each cluster?
k4$size 

#What are the company names of those in the smallest cluster?
str(k4)
data.IT$Name[k4$cluster == 2]

    
##Plot the clustering results using the fviz_cluster()
?fviz_cluster
fviz_cluster(k4, data = data.IT[, var_names], geom = c("point", "text"))





