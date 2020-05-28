library(tidyverse) 
library(corrplot)
#import the dataset
calhousing <- read.csv("C:/Users/Anna Errichiello/Documents/GitHub/progetto Machine Learning/cal-housing.csv", sep = ",")
head(calhousing)
dir.create("data")
save(calhousing, file=file.path("data","calhousing.rda"))

#convert string into numeric values
calhousing$ocean_proximity <- as.numeric(c("INLAND"=0,"NEAR BAY"= 1, "<1 OCEAN" = 0))
calhousing$ocean_proximity


# plug in your data here, I delete non numeric variable
calhousing <- data.frame(na.omit(calhousing)) 
x <- calhousing %>% select(longitude, latitude, housing_median_age,
                           total_rooms,households, median_income, 
                           median_house_value, ocean_proximity) %>% data.matrix()

y <- matrix(c(calhousing$median_house_value))

trainingIndex <- sample(nrow(calhousing), 0.90*nrow(calhousing)) # indices for 95% 
trainingData <- calhousing[trainingIndex, ] # training data

x_train <- trainingData %>% select(longitude, latitude, housing_median_age,
                                   total_rooms,households, 
                                   median_income, median_house_value, ocean_proximity) %>% data.matrix()

y_train <- matrix(c(trainingData$median_house_value))


testData <- calhousing[-trainingIndex, ] # test data
x_test <- testData %>% select(longitude, latitude, housing_median_age,
                              total_rooms,households, median_income, ocean_proximity) %>% data.matrix()

y_test <- matrix(c(testData$median_house_value))

#linear regression model
#predict on testData
H1 <- x_test%*%(1/(t(x_test)%*%x_test))%*%t(x_test) #hat matrix
predictions_test <- H1%*%y_test
show(predictions_test)
compare <- matrix(c(trainingData$median_house_value, predictions_test), ncol = 2)
compare
mean (apply(compare, 1, min)/apply(compare, 1, max)) # calculate accuracy

#predict on TrainingData
H2 <- x_train%*%(1/(t(x_train)%*%x_train))%*%t(x_train) #hat matrix
predictions_training <- H2%*%y_train

#correlation test
round(cor(x), 2) 
#correlation plots
corrplot(round(cor(x), 2)) #correlation graph
symnum(cor(x))


#notations of linear regression model
H <- x%*%(1/(t(x)%*%x))%*%t(x) #hat matrix
y_predicted <- H%*%y #predictions of y

sst <- sum((y_predicted - mean(y))^2)
sse <- sum((y_predicted - y)^2)

# R squared
rsq <- 1 - sse / sst
rsq

#ridge regression
#adapting lambdas to the submatrix
#ridge regression on matrix x
alphas <- matrix(seq(3, -2, by = -.1), ncol = 8, nrow = 8)
I <- diag(8)

 ridge_regr <- function () {
   ((t(x)%*%x+alphas%*%I)^(-1))%*%t(x)%*%x
 }
 
 ridge_regr()
 pred_ridge <- x%*%(((t(x)%*%x+alphas%*%I))^(-1))%*%t(x)%*%y
 compare <- matrix(c(calhousing$median_house_value, pred_ridge), ncol = 2)
 compare
 mean (apply(compare, 1, min)/apply(compare, 1, max)) # calculate accuracy
 
 #ridge regression on matrix x_test
 alphas <- matrix(seq(3, -2, by = -.1), ncol = 7, nrow = 7)
 I1 <- diag(7)
 
 ridge_regr_test <- function () {
   ((t(x_test)%*%x_test+alphas%*%I1)^(-1))%*%t(x_test)%*%x_test
 }
 
 ridge_regr_test()
 pred_test_ridge <- x_test%*%(((t(x_test)%*%x_test+alphas%*%I1))^(-1))%*%t(x_test)%*%y_test
 compare1 <- matrix(c(trainingData$median_house_value, pred_test_ridge), ncol = 2)
 compare1
 mean (apply(compare1, 1, min)/apply(compare1, 1, max)) # calculate accuracy
 
 #ridge regression on x_train
 alphas <- matrix(seq(3, -2, by = -.1), ncol = 8, nrow = 8)
 I2 <- diag(8)
 
 ridge_regr_train <- function () {
   ((t(x_train)%*%x_train+alphas%*%I2)^(-1))%*%t(x_train)%*%x_train
 }
 
 ridge_regr_train()
 pred_train_ridge <- x_train%*%(((t(x_train)%*%x_train+alphas%*%I2))^(-1))%*%t(x_train)%*%y_train
 compare2 <- matrix(c(trainingData$median_house_value, pred_test_ridge), ncol = 2)
 compare2
 mean (apply(compare2, 1, min)/apply(compare2, 1, max)) # calculate accuracy
 

#evaluations using notations of ridge regression
evaluations <- function (true, predicted, df) {
  sse <- sum((pred_ridge - y)^2)
  sst <- sum((pred_ridge - mean(y))^2)
  rsq <- 1 - sse / sst
  RMSE = sqrt(sse/nrow(df))
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    rsq = rsq
    
    
  )
  
}

evaluations(y_train, pred_train_ridge, trainingData)
evaluations(y_test, pred_test_ridge, testData)
