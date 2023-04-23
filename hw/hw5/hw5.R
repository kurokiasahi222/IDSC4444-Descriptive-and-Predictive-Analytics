# import packages
library(GGally)
library(caret)
library(rpart.plot)
library(gridExtra)
library(labelVector)
library(tidyverse)
library(reprex)
# Analysis  
# Processing and Visualizing data  
# a) Load the data. Get a summary of the data, report it. Use ggplot to plot a histogram 
# for the distribution of the number of bike-rides.
df <- read.csv("bike_day.csv")
summary(df)
# histogram
ggplot(df, aes(cnt_bike)) + 
  xlim(0, 9000) + 
  ylim(0, 70) + 
  geom_histogram(colour = "grey", fill = "black") + 
  ggtitle("Distrubution of the number of bike-rides") + 
  labs(x = "Number of bikes")

# The highest cnt is 8714 and the lowest is 22. It has the most count 
# around the center of the graph (3750 - 6000). Also the highest count 
# is around 60 and most counts are less than 40. 

# b) Use the function pairs() to produce a plot of 
# the relationships among count, atemp and hum. 
pairs(df[,1:3], col = "darkgreen")

# bike count and atemp seems to have a positive relationship, while 
# atemp and hum, and cnt_bike and hum does not have relationship


# c) (0.2) Split the data into 80% training and 20% testing.  
trainRows <- createDataPartition(y = df$cnt_bike, p = 0.8, list = FALSE)
train_set <- df[trainRows,]
test_set <- df[-trainRows,]

# Train a K-NN model  
# a) Decide whether you need to standardize the data or not  
# A. Yes. We need to standardize the data in K-NN. 
# We will standardize all the attributes besides cnt_bike 
# because that is our y-value. 

train_set_stand <- train_set
test_set_stand <- test_set
library(standardize)
#Apply the standardization
train_set_stand[,2:7] <- apply(train_set_stand[,2:7], MARGIN = 2, FUN = scale)
test_set_stand[,2:7]<- apply(test_set_stand[,2:7], MARGIN = 2, FUN = scale)


# b) Train a k-NN model on the appropriate attributes.

knn_model <- train(cnt_bike~., train_set_stand, method = "knn")
knn_model

# THe algorithm used k = 5, 7, 9 

# c) Get the predictions from k-NN model
knnPred <- predict(knn_model, test_set_stand)
# create a histogram of the distribution of bike rides
h_pred_knn <- ggplot(data= test_set_stand, aes(x = knnPred)) + 
  xlim(0, 9000) + 
  ylim(0, 18) +
  geom_histogram(colour = "lightblue", fill = "darkblue") +
  ggtitle("KNN, Distribution of Predicted bike rides") +
  labs(x = "Predicted bike rides")

bike_dist<- ggplot(data=test_set_stand, aes(x = cnt_bike)) + 
  geom_histogram(colour = "grey", fill = "black") +
  xlim (0,9000) + 
  ylim (0,18) + 
  ggtitle("Original Bike Distribution") +
  labs(x = "Bike rides")

grid.arrange(bike_dist, h_pred_knn, nrow=1)

# I think the performance of K-NN is OK. Even though each counts are not 
# the same, the shape of overall graph looks similar. 

#d) Computer the prediction error for the k-NN model
# create ggolit histrogram for the error
knn_error <-knnPred - test_set_stand$cnt_bike

#Visualize the prediction error
#Histogram of the distribution of the prediction error
h_error_knn = ggplot(data= test_set_stand, aes(x = knn_error)) + 
  geom_histogram(colour = "lightblue", fill = "blue") +
  xlim (-5000, 5000) + 
  ylim (0, 30) + 
  ggtitle("KNN, Distribution of Prediction Error") +
  labs(x = "Prediction Error")


#Plot prediction error vs actual price
p_error_knn<- ggplot(data = test_set_stand, aes(x=cnt_bike, y=knn_error)) +
  geom_point(size=2, color = "blue") +
  ylim (-5000, 8000) +
  xlim (0, 10000) +
  ggtitle("KNN, Prediction Error vs Actual Bike Count") +
  labs(x = "Actual Bike Count", y = "KNN Prediction Error")

grid.arrange(h_error_knn, p_error_knn)

# It seems like there are more positive errors by looking at 
# the distribution of Prediction Error. Also, KNN have more positive 
# error when the actual bike count is < 5000 and negative error when 
# actual bike count is > 5000
# We could say K-NN is over-predicting. 

#e)
knnME <- mean(knn_error)
knnME
knnRMSE<- RMSE(pred = knnPred, obs = test_set_stand$cnt_bike)
knnRMSE

# ME of 99 tells us that on average we are over-predicting by about 99. 
# RMSE of 1340 tells us that on average our prediction is off by 1340 bike counts

# Train a Regression Tree
#f) Decide whether to standardize. 
# No. We do not have to standarize the data in regression tree. 

#g) Train a regression tree
rtree <- train(cnt_bike~., train_set, method = "rpart")
rtree

# Plot the final tree 
rpart.plot(rtree$finalModel, digits=-3)

# The algorithm picked temp < 17.7, atemp < 13 for the attributes. 

# h) Get the predictions from the regression tree and use ggplot 
# to create a histogram of the distribution of the predicted bike rides
# compare it to the histogram of the true count
treePred <- predict(rtree, test_set)
h_pred_tree<- ggplot(data= test_set, aes(x = treePred)) + 
  geom_histogram(colour = "red", fill = "darkred") +
  xlim (0,9000) + 
  ylim (0, 100) + 
  ggtitle("Tree, Distribution of Predictions") +
  labs(x = "Predicted bike rides")

#compare to the actual price distribution we created above
grid.arrange(bike_dist,h_pred_tree, nrow=1)

# Regression tree only produced three bars in the graph. 
# Just by looking at the graph, regression tree does not seem to be performing well 
# because it only has three bars while the original one has a lot and seem to 
# be more complicated. 

# i) Compute the prediction error for the regression tree and 
# create a ggplot histogram for the prediction error

# Prediction error
tree_error <-treePred - test_set$cnt_bike
h_error_tree<- ggplot(data= test_set, aes(x = tree_error)) + 
  geom_histogram(colour = "darkred", fill = "red") +
  xlim (-10000, 10000) + 
  ylim (0, 30) + 
  ggtitle("Tree, Distribution of Prediction Error") +
  labs(x = "Prediction Error")

#Plot prediction error vs actual price
p_error_tree<- ggplot(data = test_set, aes(x=cnt_bike, y=tree_error)) +
  geom_point(size=2, color = "red") +
  ylim (-5000, 8000) +
  xlim (0, 10000) +
  ggtitle("Tree, Prediction Error vs Actual Bike Count") +
  labs(x = "Actual Bike Count", y = "Tree Prediction Error")

grid.arrange(h_error_tree, p_error_tree)

# The distribution of Prediction Error looks pretty uniform. But there are  
# little bit more negative errors compared to the positive one. 
# In the scatter plot we see more positive error when the actual count is 
# < 5000 and negative error when actual bike count is > 3750

# i,b) Compute the ME and RMSE for the regression tree
ME_tree <- mean(tree_error)
treeRMSE <- RMSE(pred = treePred, obs = test_set$cnt_bike)
ME_tree
treeRMSE

# ME of -178 means that on average RegressionTree under predicts
# by 178 

# RMSE of 1426 meants that on average RegressionTree's prediction 
# is off by 1426 bike counts




# Train a Linear Regression
#a) Decide whether you need to standardize the data
# A. No. I do not have to use standardized data in Linear Regression

#b) Check and comment on whether using the attributes used for the prediction. 
bike_dist
# Yes. There are some outliers, but it is somewhat normally distributed

#c) Create a correlation matrix using the attributes used for the prediction, 
cor(df[,c(2:7)])

# I will exclude atemp from the attributes because it is highly correlated 
# with temp
train_set_lr <- train_set %>% select(1:1, 3:7)
test_set_lr <- test_set %>% select(1:1, 3:7)

# d) Train a linear regression model 
lin_reg <- train(cnt_bike~., train_set_lr, method = "lm")
lin_reg

#Summarize final model
fit <- lin_reg$finalModel
options(scipen = 999) #this is to avoid scientific notation
summary(fit)
  
# j) Get the predictions from the linear regression model and 
# use ggplot to create a histogram of the distribution of the predicted 
# bike rides

lin_pred <- predict(lin_reg, newdata = test_set_lr)

#Visualize the predictions
#Create a histogram for the distribution of predicted prices
h_pred_lm <- ggplot(data= test_set_lr, aes(x = lin_pred)) + 
  geom_histogram(colour = "seagreen", fill = "darkgreen") +
  xlim (0,10000) + 
  ylim (0, 30) + 
  ggtitle("Linear Reg., Distribution of Predictions") +
  labs(x = "Predicted Bike rides")

#compare to the actual price distribution
grid.arrange(bike_dist, h_pred_lm, nrow = 1)

# By looking at the graph it seems like linear regression 
# model is doing a good job because the graph has similar 
# overall shape. But it does not capture all the details. 

#k) Compute the prediction error for the linear regression model and create 
# a ggplot histogram for the distribute of the prediction error
#Compute Prediction error
lm_error <- lin_pred - test_set_lr$cnt_bike
#Visualize the prediction error
#Histogram of the distribution of prediction errors
h_error_lm <- ggplot(data= test_set_lr, aes(x = lm_error)) + 
  geom_histogram(colour = "darkgreen", fill = "seagreen") +
  xlim (-5000, 5000) + 
  ylim (0, 30) + 
  ggtitle("Linear Reg., Distribution of Prediction Error") +
  labs(x = "Prediction Error")

#Plot of the Prediction Error vs Actual Price
p_error_lm<- ggplot(data = test_set_lr, aes(x=cnt_bike, y=lm_error)) +
  geom_point(size=2, color = "seagreen") +
  ylim (-5000, 8000) +
  xlim (0, 10000) +
  ggtitle("Linear Reg., Prediction Error vs Actual Price") +
  labs(x = "Actual Price", y = "Linear Reg. Prediction Error")

grid.arrange(h_error_lm, p_error_lm)

# By looking at these two graphs we can say that linear 
# regression is doing a good job not over predicting nor 
# under-predicting
# Also actual price and prediction error has negative 
# relationship. 

#e) 
ME_lin <- mean(lm_error)
#RMSE
lin_RMSE <- RMSE(pred = lin_pred, obs = test_set_lr$cnt_bike)
ME_lin
lin_RMSE

# ME of -56 means that on average Linear Regression model is 
# underpredicting by 56. 
# RMSE of 1428 means that on average the model is off by 
# 1428 bike counts
######
# Product Insights
#Put together the error metrics

error_table <- c(knnME, knnRMSE, ME_tree, treeRMSE, ME_lin, lin_RMSE)
names(error_table) <- c("KNN ME", "KNN RMSE", "TREE ME", "TREE RMSE", "LR ME", "LR RMSE")
error_table <- set_label(error_table, "Error table")
error_table

# Report the histogram for the distribution of the prediction errors
grid.arrange(h_error_knn,h_error_tree,h_error_lm, nrow = 1)

# I would suggest the company to either implement the K-NN model 
# or the linear Regression model. I would not suggest the company 
# to use RegressionTree model because even though the RMSE is a bit 
# lower than the Linear Regression model, its ME is -178, which means 
# it is constantly underestimating. 
# I would suggest the K-NN model because even though its ME is 99, its 
# RMSE is the lowest. Lowest RMSE means that it produces least error
# overall on average. 
# Also, I would suggest linear Regression model because it has the lowest 
# ME. It has ME of -56 which means that on average it only underestimate 
# by 56. 

