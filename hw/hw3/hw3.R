### import libraries
library(stats)
library(factoextra)
library(ggplot2)
#a) 
telco_data <- read.csv("TelcoData.csv")
var_names <- c("Age", "Tenure.Months", "Monthly.Charges")
summary(telco_data[,var_names])
# Age -> Mean: 50.48, Min: 19, Max:80
# Age is skewed toward Max, which makes sense because
# adults and parents pay bills most of the time
# Tenure.Months -> Mean: 32.36, Min: 1, Max: 72
# Average months are 32 months which means about 3 years
# Longest months are 72 which is about 6 years. It is shorter
# that I expected.
# Montly.Charges -> Mean: 91.34, Min: 68.95, Max: 118.35
# Mean for Monthly charges is $91 which is pretty much in
# between min and max.

# b) use ggplot to produce histogram for Tenure
?ggplot
ggplot(data = telco_data, aes(x= Tenure.Months, fill = ..count..)) + 
  geom_histogram(alpha=1) + 
  ggtitle("User Count by Total amount of months their been with Company") + 
  labs(x = "Months", y = "User Count")
# 
# The histogram is skewed toward the both side of the graph.
# There is a lot of user count in the first couple of months but
# gradually decrease when we go towards the middle, and go back up
# again as we pass the midpoint.

# c)
ggplot(data = telco_data, aes(x= Partner_cat, y = Monthly.Charges, 
                              group = Partner_cat )) + 
  geom_boxplot(outlier.colour="orange", outlier.shape=2, outlier.size=3) + 
  ggtitle("Monthly Charges by whether the customer 
          lives with a partner") + 
  labs(x = "Lives with a partner", y = "Montly Charges")

# The mean value for monthly charges is higher for customers
# who lives with his or her partner. There are no outliers.

# d)
# exclude customer id and partner_cat for the cluster analysis

# e) Assess where you need to normalize the data. 
# YES we need to normalize the data because
# 1. We are dealing with different data types
# 2. Total Charges have larger numbers than any other ones.

min_max_norm = function(x){
  return ((x - min(x))/(max(x) - min(x)))}
telco_norm <-telco_data
norm_column <- c(2:15)
telco_norm[, norm_column] <- apply(telco_norm[, norm_column], MARGIN = 2, FUN = min_max_norm)

### Hierarchical Clustering

# f) Generate distance matrix
dist_matrix_norm <- dist(telco_norm[, norm_column], method = "euclidian")
View((as.matrix(dist_matrix_norm)[1:5, 1:5]))


# g) Run hierarchical clustering

hc <- hclust(dist_matrix_norm, method = "ward.D")

# h) Plot the dendrogram
plot(hc, hang = 0, labels = FALSE)

# i) use rect.hclust to draw 
rect.hclust(hc, k = 4, border = 2:5)

# j) cut the dendogram into 4 clusters
hc_4 <- cutree(hc, k = 4)
table(hc_4)
# Cluster 1: 171
# Cluster 2: 337
# Cluster 3: 497
# Cluster 4: 144


# k)
telco_data$hc_4 <- hc_4
library(plyr)
ddply(telco_data, .(hc_4), summarize, n = length(CustomerID), 
      Partner = sum(Partner) / n, senior_citizen = sum(Senior.Citizen) / n,
      online_backup = sum(Online.Backup) /n, tech_support = sum(Tech.Support) / n, 
      streaming_movies = sum(Streaming.Movies) / n, 
      streaming_tv = sum(Streaming.TV) /n, 
      online_security = sum(Online.Security) / n, 
      unlimitated_data = sum(Unlimited.Data) /n, 
      Montly_mean=mean(Monthly.Charges), tenure_mean=mean(Tenure.Months), 
      age_mean = mean(Age))

### K-means Clustering
set.seed(123)
# I) Run four different versions of k-means clustering
library(gridExtra)
k2 <- kmeans(telco_norm[, norm_column], centers = 2, nstart = 15)
k4 <- kmeans(telco_norm[, norm_column], centers = 4, nstart = 15)
k6 <- kmeans(telco_norm[, norm_column], centers = 6, nstart = 15)
k8 <- kmeans(telco_norm[, norm_column], centers = 8, nstart = 15)

# m) use fviz_cluster to visualize
p2 <- fviz_cluster(k2, geom = "point", data = telco_norm[, norm_column]) + ggtitle("k = 2")
p4 <- fviz_cluster(k4, geom = "point", data = telco_norm[, norm_column]) + ggtitle("k = 4")
p6 <- fviz_cluster(k6, geom = "point", data = telco_norm[, norm_column]) + ggtitle("k = 6")
p8 <- fviz_cluster(k8, geom = "point", data = telco_norm[, norm_column]) + ggtitle("k = 8")
grid.arrange(p2, p4, p6, p8, nrow = 2)
# k = 2 looks the most appropriate because it has lowest overlap. 
# When k > 2 clusters have too much overlap. 

# n) find out appropriate cluster by computing WSS
WSS_curve <- c()
for (n in 1:10){
  k = kmeans(telco_norm[,var_names], centers = n, nstart = 10)
  wss = k$tot.withinss
  WSS_curve[n] <- wss
} 
plot(1:10, WSS_curve, type = "b", col = "red", ylab = "WSS", xlab = "K", ylim = c(0,350) )

# o) run again k-means using the number of clusters you decided in n







