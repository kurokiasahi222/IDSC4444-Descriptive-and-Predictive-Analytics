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
data_stand <- data

#apply the standardization to the attributes you included in the var_names
data_stand[, var_names] <- apply(data_stand[, var_names], MARGIN = 2, FUN = scale)

#Check if all the attributes you standardized have a mean 0
summary(data_stand[, var_names])

#----[Hierarchical Clustering]
# First create a dataframe data_IT with only companies in the Information Technology sector.
data_IT <- data_stand[data_stand$Sector=="Information Technology",]

sub_IT <- subset(data_stand, data_stand$Sector == "Information Technology")

#Rename the rows to company symbol, this should help you identify the distance matrix cells
rownames(data_IT) <- data_IT$Symbol

#Create a distance matrix for the newly filtered dataset using the manhattan measure. 
#What variables should you use?

d_mat <- dist(data_IT[,var_names],method = "manhattan")

#View the matrix
View(as.matrix(d_mat))

# Use the hclust() to generate hierarchical clusters from the d_mat distance matrix
#use the ward method
h <- hclust(d_mat, method = "ward.D")

#Plot the dendogram from the clusters
plot(h, labels = sub_IT$Symbol, hang = -1, cex = 0.4)

#Cut the tree into 5 clusters and plot these 
rect.hclust(h, k = 5, border = 2:4)


#Assign each company a cluster number and find out which cluster is ORCL in?
clusters <- cutree(h,k = 5)
clusters
table(clusters)
#add a column to your dataset with the cluster's numbers
data_IT$clusters <- clusters

#Summarize cluster 1 and 4 to get an idea of how they differ from each other

c1<- (subset(data_IT[,var_names], data_IT$clusters == 1))
c4<- (subset(data_IT[,var_names], data_IT$clusters == 4))


#----[K-Means]--
# Implement k-means clustering using the standardized data for IT companies as above
#pick a number of centers

set.seed(546)
k <- kmeans(data_IT[,var_names],centers = 5)

#what are the sizes of the clusters?
k$size

# Create the WSS curve by running kmeans for different number of clusters
wss <- c()
for(i in 1:10){
  k <- kmeans(data_IT[,var_names],centers = i)
  wss[i] <- k$tot.withinss}

plot(1:10,y = wss,type="o",col="red", ylab = "", xlab = "K", main = "WSS")

#After evaluating the Elbow plot, rerun kmeans with what you think is the most appropriate number of clusters. 

k4 <- kmeans(data_IT[,var_names], centers = 4)
#How many points in each cluster?
k4$size

#What are the company names of those in the smallest cluster?
data_IT$Name[k4$cluster == 1]
#where I put 1, you should put the cluster number with the smallest number of companies

##Plot the clustering results using the fviz_cluster()
fviz_cluster(k4, data=data_IT[,var_names])


