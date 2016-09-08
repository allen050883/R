# R
library(plyr)
library(dplyr)
library(randomForest)
glimpse(data_1)
#cross validation, using rf to predict sepal.length
k = 11

data_1$id <- sample(1:k, nrow(data_1), replace = TRUE)
list <- 1:k
# prediction and test set data frames that we add to with each iteration over
# the folds
prediction <- data.frame()
testsetCopy <- data.frame()
#Creating a progress bar to know the status of CV
progress.bar <- create_progress_bar("text")
progress.bar$init(k)
#function for k fold
for(i in 1:k){
  # remove rows with id i from dataframe to create training set
  # select rows with id i to create test set
  trainingset <- subset(data_1, id %in% list[-i])
  testset <- subset(data_1, id %in% c(i))
  
  #run a random forest model
  mymodel <- randomForest(trainingset$Demanda_uni_equil ~ ., data = trainingset, ntree = 100)
  
  #remove response column 1, Sepal.Length
  temp <- as.data.frame(predict(mymodel, testset[,-1]))
  
  # append this iteration's predictions to the end of the prediction data frame
  prediction <- rbind(prediction, temp)
  
  # append this iteration's test set to the test set copy data frame
  # keep only the Sepal Length Column
  testsetCopy <- rbind(testsetCopy, as.data.frame(testset[,1]))
  
  progress.bar$step()
}
# add predictions and actual Sepal Length values
result <- cbind(prediction, testsetCopy[, 1])
names(result) <- c("Predicted", "Actual")
result$Difference <- abs(result$Actual - result$Predicted)
# As an example use Mean Absolute Error as Evalution 
summary(result$Difference)

