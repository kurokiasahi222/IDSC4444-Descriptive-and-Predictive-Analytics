
#Install and Load the following packages
install.packages("GGally")
install.packages("labelVector")
install.packages("tidyverse")
library(GGally)
library(caret)
library(rpart.plot)
library(gridExtra)
library(labelVector)
library(tidyverse)

#Load the dataset ToyotaCorolla
df <- read.csv("ToyotaCorolla.csv")

#Take a look at the data
summary(df)

#Let us check the distribution of Price
ggplot(data=df, aes(x = Price)) + 
        geom_histogram(colour = "grey", fill = "black") +
        xlim (0,35000) + 
        ylim (0,400) + 
        ggtitle("Original Price Distribution") +
        labs(x = "Price")

#Let us check relationship among attributes
#We can use scatterplots
plot(df$Age, df$Price, col = "darkgreen", 
     xlim = c(0, 90), ylim = c(0, 35000), ylab = "Price", 
     xlab = "Age", main = "Scatterplot of Price and Age")

#A more efficient way to see relatioships among multiple attributes at a time
#is using the pairs function
pairs(df[,1:3], col = "darkgreen")

#Prepare the data for prediction
#Split the data into training and testing
trainRows <- createDataPartition(y = df$Price, p = 0.7, list = FALSE)

train_set <- df[trainRows,]
test_set <- df[-trainRows,]

#Standardize the data - do not standardize the Y
#First, create a copy of the datasets
train_set_stand <- train_set
test_set_stand <- test_set

library(standardize)

#Apply the standardization
train_set_stand[,2:10] <- apply(train_set_stand[,2:10], MARGIN = 2, FUN = scale)
test_set_stand[,2:10] <- apply(test_set_stand[,2:10], MARGIN = 2, FUN = scale)

#Train knn using the function train() and specifying as method knn
#the algorithm will look at the outcome variable and understand whether 
#it is a classification or a numerical prediction task
#remember we run knn using the standardized data
knn_model <- train(Price~., train_set_stand, method = "knn")
knn_model 

#Get the predictions from the knn_model
knnPred <- predict(knn_model, test_set_stand)
knnPred
#Get a histogram of the predicted prices
h_pred_knn<- ggplot(data= test_set_stand, aes(x = knnPred)) + 
        geom_histogram(colour = "lightblue", fill = "darkblue") +
        xlim (0,35000) + 
        ylim (0,200) + 
        ggtitle("KNN, Distribution of Predicted Price") +
        labs(x = "Predicted Price")

#Compare with the original price distribution
price_dist<- ggplot(data=test_set_stand, aes(x = Price)) + 
        geom_histogram(colour = "grey", fill = "black") +
        xlim (0,35000) + 
        ylim (0,200) + 
        ggtitle("Original Price Distribution") +
        labs(x = "Price")


grid.arrange(price_dist, h_pred_knn, nrow=1)


#Error Metrics
#Compute the prediction error
knn_error <-knnPred - test_set_stand$Price

#Visualize the prediction error
#Histogram of the distribution of the prediction error
h_error_knn = ggplot(data= test_set_stand, aes(x = knn_error)) + 
        geom_histogram(colour = "lightblue", fill = "blue") +
        xlim (-10000, 10000) + 
        ylim (0, 150) + 
        ggtitle("KNN, Distribution of Prediction Error") +
        labs(x = "Prediction Error")


#Plot prediction error vs actual price
p_error_knn<- ggplot(data = test_set_stand, aes(x=Price, y=knn_error)) +
        geom_point(size=2, color = "blue") +
        ylim (-5000, 8000) +
        xlim (0, 30000) +
        ggtitle("KNN, Prediction Error vs Actual Price") +
        labs(x = "Actual Price", y = "KNN Prediction Error")

grid.arrange(h_error_knn, p_error_knn)


#Mean error: take the mean of the prediction error
knnME <- mean(knn_error)
knnME
#MAE - Mean absolute error: use th MAE function
knnMAE <- MAE(pred = knnPred, obs = test_set_stand$Price)
knnMAE
#MAPE - Mean absolute % error
#First, take the absolute value of the ratio between error and actual value
knn_abs_relative_error <-abs(knn_error/test_set_stand$Price)
#Then take the average and * by 100
knnMAPE <- mean(knn_abs_relative_error)*100
knnMAPE
#RMSE - Root mean square error: use the RMSE function
knnRMSE<- RMSE(pred = knnPred, obs = test_set_stand$Price)
knnRMSE

#Put together the error measures
res_knn<- c(knnME, knnMAE, knnMAPE, knnRMSE)
names(res_knn) <-c("ME", "MAE", "MAPE", "RMSE")
res_knn <- set_label(res_knn, "KNN")
res_knn

#Regression tree
#Train regression tree; use the non standardized data
rtree <- train(Price~., train_set, method = "rpart")
rtree

#Plot the tree
rpart.plot(rtree$finalModel, digits=-3)

#Get predictions using the testing data
treePred <- predict(rtree, test_set)

#Get a histogram of the predicted prices
h_pred_tree<- ggplot(data= test_set, aes(x = treePred)) + 
        geom_histogram(colour = "red", fill = "darkred") +
        xlim (0,30000) + 
        ylim (0, 300) + 
        ggtitle("Tree, Distribution of Predictions") +
        labs(x = "Predictions")

#compare to the actual price distribution we created above
grid.arrange(price_dist,h_pred_tree, nrow=1)


#Error Metrics
#Prediction error
tree_error <-treePred - test_set$Price

#Visualize prediction error
#Histogram of the distribution of prediction error
h_error_tree<- ggplot(data= test_set, aes(x = tree_error)) + 
        geom_histogram(colour = "darkred", fill = "red") +
        xlim (-10000, 10000) + 
        ylim (0, 150) + 
        ggtitle("Tree, Distribution of Prediction Error") +
        labs(x = "Prediction Error")

#Plot prediction error vs actual price
p_error_tree<- ggplot(data = test_set, aes(x=Price, y=tree_error)) +
        geom_point(size=2, color = "red") +
        ylim (-5000, 8000) +
        xlim (0, 30000) +
        ggtitle("Tree, Prediction Error vs Actual Price") +
        labs(x = "Actual Price", y = "Tree Prediction Error")

grid.arrange(h_error_tree, p_error_tree)

#Compute other metrics
#Mean error
ME_tree <- mean(tree_error)
#MAE
treeMAE <- MAE(pred = treePred, obs = test_set$Price)
#MAPE
tree_abs_relative_error <-abs(tree_error/test_set$Price)
treeMAPE <- mean(tree_abs_relative_error)*100
#RMSE
treeRMSE <- RMSE(pred = treePred, obs = test_set$Price)

#Put together the performance measures
res_tree<- c(ME_tree, treeMAE, treeMAPE, treeRMSE)
names(res_tree) <-c("ME", "MAE", "MAPE", "RMSE")
res_tree <- set_label(res_tree, "Regression Tree")
res_tree


#Linear Regression
#check whether assumptions seem to hold
#we had already looked at the distribution of the outcome variable, price
#and at scatterplots of price with other attributes
#we need to check at the correlation between attributes to see if there is
#a collinearity problem
#Use the cor function
#Default method is the Pearson coefficient: from -1 to 1
#close to zero means low correlation
cor(df[,c(2:10)])


#use the not normalized data
#exclude attributes that are collinear
#in this case, we exclude CC, that is heavily correlated with Weight and
#Fuel_Diesel
#To make things easier when we train the linear regression,
#we can create copy of the data that excludes the attributes we do not want
#to include
train_set_lr <- train_set %>% select(1:6, 8:10)
test_set_lr <- test_set %>% select(1:6, 8:10)

#train the linear regression
#use "lm" as method
#use the dataset we just created
lin_reg <- train(Price~., train_set_lr, method = "lm")
lin_reg

#Summarize final model
fit <- lin_reg$finalModel

#Look at the coefficients of the linear regression
options(scipen = 999) #this is to avoid scientific notation
summary(fit)

#Get predictions using the testing data for the linear regression
lin_pred <- predict(lin_reg, newdata = test_set_lr)

#Visualize the predictions
#Create a histogram for the distribution of predicted prices
h_pred_lm <- ggplot(data= test_set_lr, aes(x = lin_pred)) + 
        geom_histogram(colour = "seagreen", fill = "darkgreen") +
        xlim (0,30000) + 
        ylim (0, 200) + 
        ggtitle("Linear Reg., Distribution of Predictions") +
        labs(x = "Predicted Price")

#compare to the actual price distribution
grid.arrange(price_dist, h_pred_lm, nrow = 1)


#Error metrics
#Compute Prediction error
lm_error <- lin_pred - test_set_lr$Price

#Visualize the prediction error
#Histogram of the distribution of prediction errors
h_error_lm <- ggplot(data= test_set_lr, aes(x = lm_error)) + 
        geom_histogram(colour = "darkgreen", fill = "seagreen") +
        xlim (-10000, 10000) + 
        ylim (0, 150) + 
        ggtitle("Linear Reg., Distribution of Prediction Error") +
        labs(x = "Prediction Error")

#Plot of the Prediction Error vs Actual Price
p_error_lm<- ggplot(data = test_set_lr, aes(x=Price, y=lm_error)) +
        geom_point(size=2, color = "seagreen") +
        ylim (-5000, 8000) +
        xlim (0, 30000) +
        ggtitle("Linear Reg., Prediction Error vs Actual Price") +
        labs(x = "Actual Price", y = "Linear Reg. Prediction Error")

grid.arrange(h_error_lm, p_error_lm)


#Compute other metrics
#Mean error
ME_lin <- mean(lm_error)
#MAE
lin_MAE <- MAE(pred = lin_pred, obs = test_set_lr$Price)
#MAPE
lin_abs_relative_error <-abs(lm_error/test_set_lr$Price)
linMAPE <- mean(lin_abs_relative_error)*100
#RMSE
lin_RMSE <- RMSE(pred = lin_pred, obs = test_set_lr$Price)

#Put together the error metrics
res_lin<- c(ME_lin, lin_MAE, linMAPE, lin_RMSE)
names(res_lin) <-c("ME", "MAE", "MAPE", "RMSE")
res_lin <- set_label(res_lin, "Linear Regression")
res_lin


#compare the three models
#Compare the distribution of original price with the distributions of predicted prices
grid.arrange(price_dist, h_pred_knn,h_pred_tree,h_pred_lm, nrow = 2)

#Compare the scatterplots of the Prediction Error vs Actual Price
#for the three models
grid.arrange(p_error_knn,p_error_tree,p_error_lm, nrow = 1)

#Compare the histograms of the distribution of prediction errors
#for the three models
grid.arrange(h_error_knn,h_error_tree,h_error_lm, nrow = 1)

#Compare the error metrics
res_knn
res_tree
res_lin
