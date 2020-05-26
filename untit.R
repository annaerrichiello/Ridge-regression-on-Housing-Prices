library(tidyverse) 
library(broom)
library(car)
library(corrplot) 
#import the dataset
calhousing <- read.csv("C:/Users/Anna Errichiello/Documents/GitHub/progetto Machine Learning/cal-housing.csv", sep = ",")
head(calhousing)
dir.create("data")
save(calhousing, file=file.path("data","calhousing.rda"))



# plug in your data here, I delete non numeric variable
calhousing <- data.frame(na.omit(calhousing)) [-10] 

x <- calhousing %>% select(longitude, latitude, housing_median_age,
                          total_rooms,households, median_income, 
                          median_house_value) %>% data.matrix()
t(x)
y <- matrix(c(calhousing$median_house_value), nrow = 20433, ncol = 1)
round(cor(x), 2) # Correlation Test
corrplot(round(cor(x), 2)) #correlation graph
symnum(cor(x))
heatmap(cor(x), y)

trainingIndex <- sample(nrow(calhousing), 0.99*nrow(calhousing)) # indices for 99,9% 
trainingData <- calhousing[trainingIndex, ] [-5,-10] # training data
x_train <- trainingData %>% select(longitude, latitude, housing_median_age,
                                             total_rooms,households, 
                                   median_income, median_house_value) %>% data.matrix()
y_train <- trainingData$median_house_value

testData <- calhousing[-trainingIndex, ] # test data
x_test <- testData %>% select(longitude, latitude, housing_median_age,
                               total_rooms,households, median_income) %>% data.matrix()
y_test <- matrix(c(testData$median_house_value), nrow = 205, ncol = 1) 

lmMod <- lm(median_house_value ~ ., trainingData)  # the linear reg model
summary (lmMod) # get summary
vif(lmMod) # get VIF

predicted <- predict (lmMod, testData)  # predict on test data
compare <- cbind(actual=testData$median_house_value, predictions)  # combine actual and predicted
show (compare)

mean (apply(compare, 1, min)/apply(compare, 1, max)) # calculate accuracy


ridge_regr <- linearRidge(median_house_value ~ ., data = trainingData)  # the ridge regression model
summary(ridge_regr)

predicted <- predict(ridge_regr, testData)  # predict on test data
compare <- cbind (actual=testData$median_house_value, predicted)  # combine
compare
mean (apply(compare, 1, min)/apply(compare, 1, max))

#ridge regression

lambdas <- matrix(seq(3, -2, by = -.1), ncol = 7, nrow = 7)

alphas <- matrix(c(0.1,1.0,10), ncol = 9)


#We can automatically find a value for lambda that is optimal by using 
#cv.glmnet() as follows:
cv_ridge <- cv.glmnet(x_train, y_train, alpha = alphas, lambda = lambdas) 
show (cv_ridge)
plot(cv_ridge)

#The lowest point in the curve indicates the optimal lambda: the log value 
#of lambda that best minimised the error in cross-validation. 
#We can extract this values as:
opt_lambda <- ridge$lambda.min
opt_lambda

#I substitute the optimal lamda in the ridge regression
ridge_regr_opt_lambda = glmnet(x_train, y_train, family = "gaussian", offset = NULL,
                    nlambda = 100,lambda = opt_lambda,
                    na.rm=TRUE, relaxed = FALSE)
show(ridge_regr_opt_lambda)

y_predicted <- predict(ridge_regr, s = opt_lambda, newx = x)

# Sum of Squares Total and Error
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

# R squared
rsq <- 1 - sse / sst
rsq
sprintf("%s", rsq)

#model using eval_results
eval_results <- function (true, predicted, df) {
  sse <- sum((predicted - y)^2)
  sst <- sum((y - mean(y))^2)
  rsq <- 1 - sse / sst
  RMSE = sqrt(sse/nrow(df)) %>% data.frame()
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    rsq = sprintf("%s", rsq) #voglio stamparlo con tot cifre significative
  
    
  )
  
}
# Prediction and evaluation on train data
predictions_trainingData <- predict(ridge_regr, s = opt_lambda, newx = x_train) %>% data.frame()
eval_results(y_train, predictions_trainingData, trainingData)


# Prediction and evaluation on test data
predictions_testData <- predict(ridge_regr, s = opt_lambda, newx = x_test) 
eval_results(y_test, predictions_testData, testData)


#'Principal Component Analysis
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples

pca= function(x) {
  res = prcomp(x)
}
show(pca(x))
