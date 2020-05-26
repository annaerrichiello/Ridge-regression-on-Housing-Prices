#'
#' @author Anna Errichiello <anna.errichiello@studenti.unimi.it>
#'
#' @export
#'
#'
#'
#'
#'
  
library(caret)
library(randomForest)  
data("calhousing")
# create new dataset without missing data
newdata <- na.omit(calhousing)
newdata

# Define train control for k fold cross validation
train_control <- trainControl(method="cv", number=10)
# Fit Naive Bayes Model
model <- train(median_house_value~., data=newdata, trControl=train_control, method = "glm")
# Summarise Results
print(model)


list.of.fits <- list()
for (i in 0:10) {
  fit.name <- paste0("alpha", i/10)
  eval <-
    ridge_regr_train()
}


results <- data.frame()
for (i in 0:10) {
  fit.name <- paste0("alpha", i/10)
  
  ## Use each model to predict 'y' given the Testing dataset
  eval <- 
    evaluations(y_train, predictions_trainingData, trainingData)
  
  ## Calculate the Mean Squared Error...
  mse <- mean((y_test - predictions_test)^2)
  
  ## Store the results
  temp <- data.frame(alpha=i/10, mse=mse, fit.name=fit.name)
  results <- rbind(results, temp)
}

view(results)


