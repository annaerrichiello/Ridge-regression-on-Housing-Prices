library(tidyverse) 
library(corrplot)
#import the dataset
calhousing <- read.csv("C:/Users/Anna Errichiello/Documents/GitHub/progetto Machine Learning/cal-housing.csv", sep = ",")
head(calhousing)
dir.create("data")
save(calhousing, file=file.path("data","calhousing.rda"))

#convert string into numeric values
calhousing$ocean_proximity <- as.character(calhousing$ocean_proximity)
calhousing$ocean_proximity[which(calhousing$ocean_proximity=="INLAND")] <- 0
calhousing$ocean_proximity[which(calhousing$ocean_proximity=="NEAR BAY")] <- 1
calhousing$ocean_proximity[which(calhousing$ocean_proximity=="<1H OCEAN")] <- 0
calhousing$ocean_proximity


# plug in your data here, I delete non numeric variable
calhousing <- data.frame(na.omit(calhousing)) 
x <- calhousing %>% select(longitude, latitude, housing_median_age,
                           total_rooms,households, median_income, 
                           median_house_value, ocean_proximity) %>% data.matrix()
x[is.na(x)] <- mean(x, na.rm = TRUE)

y <- matrix(c(calhousing$median_house_value))

trainingIndex <- sample(nrow(calhousing), 0.50*nrow(calhousing)) # indices for 95% 
trainingData <- calhousing[trainingIndex, ] # training data

x_train <- trainingData %>% select(longitude, latitude, housing_median_age,
                                   total_rooms,households, 
                                   median_income, median_house_value, ocean_proximity) %>% data.matrix()
x_train[is.na(x_train)] <- mean(x_train, na.rm = TRUE)
y_train <- matrix(c(trainingData$median_house_value))


testData <- calhousing[-trainingIndex, ] # test data
x_test <- testData %>% select(longitude, latitude, housing_median_age,
                              total_rooms,households, median_income, ocean_proximity) %>% data.matrix()
x_test[is.na(x_test)] <- mean(x_test, na.rm = TRUE)
y_test <- matrix(c(testData$median_house_value))

#predict on testData
H1 <- x_test%*%(1/(t(x_test)%*%x_test))%*%t(x_test) #hat matrix
predictions_test <- H1%*%y_test
show(predictions)
compare <- matrix(c(trainingData$median_house_value, predictions), ncol = 2)
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


#notations of ridge regression to find predicted values
H <- x%*%(1/(t(x)%*%x))%*%t(x) #hat matrix
y_predicted <- H%*%y #predictions of y

sst <- sum((y_predicted - mean(y))^2)
sse <- sum((y_predicted - y)^2)

# R squared
rsq <- 1 - sse / sst
rsq

#adapting lambdas to the submatrix
lambdas <- matrix(seq(3, -2, by = -.1), ncol = 8, nrow = 8)
I <- diag(8)

 ridge_regr <- function () {
   t(x)%*%x+lambdas%*%I
 }
 
 ridge_regr()
 
 
 lambdas <- matrix(seq(3, -2, by = -.1), ncol = 7, nrow = 7)
 I1 <- diag(7)
 
 ridge_regr_test <- function () {
   t(x_test)%*%x_test+lambdas%*%I1
 }
 
 ridge_regr_test()
 
 lambdas <- matrix(seq(3, -2, by = -.1), ncol = 8, nrow = 8)
 I2 <- diag(8)
 
 ridge_regr_train <- function () {
   t(x_train)%*%x_train+lambdas%*%I2
 }
 
 ridge_regr_train()
 
 
#find the optimal lambda
alphas <- seq(0.1,1.0,10)


evaluations <- function (true, predicted, df) {
  sse <- sum((y_predicted - y)^2)
  sst <- sum((y_predicted - mean(y))^2)
  rsq <- 1 - sse / sst
  RMSE = sqrt(sse/nrow(df))
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    rsq = rsq
    
    
  )
  
}

evaluations(y_train, predictions_trainingData, trainingData)
evaluations(y_test, predictions_test, testData)

list.of.fits <- list()
for (i in 0:10) {
  fit.name <- paste0("alpha", i/10)
  eval <-
    ridge_regr_train()
}

#evaluate dependences on alpha
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
plot(results)

pca= function(x) {
  res = prcomp(t(x)%*%x+lambdas%*%I)
}
show(pca(x))


ridge <- function(lambda) {
  x = x_train
  y = y_train
  lambda = lambdas
  alpha = alphas
}

cla.pca <- prcomp(x , center = TRUE,scale. = TRUE)

summary(cla.pca)

pc1 <- cla.pca$rotation[,1]
pc2 <- cla.pca$rotation[,2]
pc3 <- cla.pca$rotation[,3]
housepc <- rbind(pc1,pc2,pc3,calhousing$median_house_value)

mean(apply(housepc, 1, min)/apply(housepc, 1, max))
