
#Install and Load the following packages
install.packages("caret")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("e1071")
library("caret")
library("e1071")
library("rpart")
library("rpart.plot")


#Load the dataset customer_churn
data = read.csv("customer_churn.csv")

#OBJECTIVE: build classification models to predict whether a customer will churn
#or not


#First, we need to randomly split our dataset into training and testing dataset
#Use the createDataPartition function
#y identifies our class (outcome) variable; in this case, it is the Churn. variable
#p is the % of datapoints we want in the training data; 
#list = FALSE is to specify that our outcome is not a list
#Note: since the partition is randomly created, the results may be slightly different
#for each of us

#createDataPartition is going to randomly pick 70% of our existing observations
#and put them into the vector train_rows
?createDataPartition
train_rows <- createDataPartition(y = data$Churn., p =0.7, list = FALSE)

#Create the training data using the randomly picked observations from before
data_train <- data[train_rows,]

#Create the test data, by taking all the remaining observations that 
#were not included in data_train
data_test <- data[-train_rows, ]

#Next, we need to normalize the data
#In this case, we use standardization
#First, let us create a copy of the training and testing data

data_train_stand <- data_train
data_test_stand <- data_test

#load the package standardize
library(standardize)

#Think about which variables we want to standardize
#remember: we do not standardize the outcome (class) variable
#So, we need to exclude the variable Churn.
#we will standardize variables from 1 to 17

#We apply the function scale to the data_train_stand and data_test_stand
data_train_stand[,1:17] <- apply(data_train_stand[,1:17], MARGIN = 2, FUN = scale)
data_test_stand[,1:17] <- apply(data_test_stand[,1:17], MARGIN = 2, FUN = scale)

#K-NN
#Let us train a k-NN classifier on the standardize, training data
#Use the function train(), specify the method knn, and specify the class variable (Y)
#and attributes you want to use for prediction. If you want to use all the attributes
#simply specify Churn.~.
#pay attention, sometimes not all attributes should be used 
?train
fitKNN <- train(data= data_train_stand, method = "knn", Churn.~.)
fitKNN

#We can set the values of k we would like the method to use
grid = expand.grid(k = c(3,5,7,9,12))

fitKNN <- train(data= data_train_stand, method = "knn", Churn.~., 
                trControl = trainControl(search="grid"), tuneGrid=grid)
fitKNN

#Plot how accuracy of the model changes with number of k
#Note: since the model is trained on a random set of data, we may get slightly different results 
plot(fitKNN, ylab = "Accuracy")

#Use the k-NN model we just trained to predict the classes of the observations in the 
#testing data
#in other words, we are going to use the model we built on the testing data
#and try to use it to predict the class (outcome) of the observations in the testing data
#note that since we also know the "real" value for the class variable for the observations
#in the testing data, we can then compare the predictions obtained with the model with the
#real value to evaluate how good our model is
knn_predictions <- predict(fitKNN, data_test_stand)

#Create the confusionMatrix to evaluate the performance of the model
#we use the function confusionMatrix and input the predictions we want to use to create the matrix
#The the class (outcome) variable
#the mode option controls what type of measures we get
#if we use mode = "prec_recall" we will get precision, recall and F1 for the class
#that has been set as the "positive" class

confusionMatrix(knn_predictions, as.factor(data_test_stand$Churn.), mode = "prec_recall")

#we can choose which class to use as the "positive" class with the option positive
#in this case, the option positive = "True." will set the class True as the positive class
#as such, the measures we will get from the matrix refer to the class True

confusionMatrix(knn_predictions, as.factor(data_test_stand$Churn.), mode = "prec_recall", positive = "True.")

#If we want to get the measures for both classes, we can avoid the option mode
#Note: in this case, to distinguish between the two classes, the matrix will use the terms
#Sensitivity, Specificity, PPV and NPV. 
#Sensitivity and PPV will refer to the class defined as "positive", that is reported at the 
#bottom of the output
#Specificity and NPV will refer to the other class

confusionMatrix(knn_predictions, data_test_stand$Churn.)

#Training a Decision Tree
#We still use the function train(), specify as method rpart (recursive partitioning)
#Also, we do not need to use the standardized data with decision trees

fitDT <- train(data = data_train, method = "rpart", Churn.~.)


#To see a description of the tree 
fitDT$finalModel

#See the decision rules
rpart.rules(fitDT$finalModel)

#Get the plot of the tree
rpart.plot(fitDT$finalModel)

#get the predictions from the tree
DT_predictions <- predict(fitDT$finalModel, newdata = data_test, type = "class")

#Create the confusionMatrix to evaluate the performance of the model
confusionMatrix(DT_predictions, as.factor(data_test$Churn.), mode = "prec_recall")

#we can choose which class to use as the "positive" class with the option positive
confusionMatrix(DT_predictions, as.factor(data_test$Churn.), mode = "prec_recall", positive = "True.")

#If we want to get the measures for both classes, we can avoid the option mode
confusionMatrix(DT_predictions, as.factor(data_test$Churn.))

#Rather than getting the class prediction, we can get the probabilities.
#that is, we can get what is the probability that a certain observation belongs to one class or the other
#by default, a cut-off of 50% is used
#Nevertheless, we can change the cutoff value depending on the context
#Example: in this case, we want to be sure to not miss any potential Churn
#In other words, we want to avoid to classify a customer as False Churn if there is a decent
#probability that is a True Churn. We care about the recall for the class True more than the overall
#accuracy of the model or precision.
#We can increase the cut-off for False., so that the algorithm will classify a customer 
#as False Churn only if the probability is really high

#First, get the predicted probabilities
#we still use the function predict, but now we specify type = "prob"
#also, we save the results in a dataframe
DT_prob<- as.data.frame(predict(fitDT$finalModel, newdata = data_test, type = "prob"))

#next, we create a new column in the dataframe just created that is going to assign to each
#observation "False" or "True" based on the cutoff
#in this example, we specify that if the Probablity of being False is > 0.87, then assign False
#if it is lower, then we assign True
DT_prob$pred_class <- ifelse(DT_prob$False. > 0.87, "False.", "True.")

#transform the new created column into a class variable by using the as.factor()
DT_prob$pred_class<- as.factor(DT_prob$pred_class)

#create again the confusion matrix, in any format preferred
confusionMatrix(DT_prob$pred_class, as.factor(data_test$Churn.))

confusionMatrix(DT_prob$pred_class, as.factor(data_test$Churn.), mode = "prec_recall")

confusionMatrix(DT_prob$pred_class, as.factor(data_test$Churn.),mode = "prec_recall", positive = "True.")




