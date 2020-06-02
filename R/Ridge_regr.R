#ridge regression
#adapting lambdas to the submatrix
#ridge regression on matrix x
alphas <- matrix(seq(0.0, 1.0, by = .1), ncol = 8, nrow = 8)
I <- diag(8)

ridge_regr <- function () {
  ((t(x)%*%x+alphas%*%I)^(-1))%*%t(x)%*%y
}

ridge_regr()
pred_ridge <- x%*%(((t(x)%*%x+alphas%*%I))^(-1))%*%t(x)%*%y
compare <- matrix(c(calhousing$median_house_value, pred_ridge), ncol = 2)
compare

#ridge regression on matrix x_test
alphas <- matrix(seq(0.0, 1.0, by = .1), ncol = 8, nrow = 8)
I1 <- diag(8)

ridge_regr_test <- function () {
  ((t(x_test)%*%x_test+alphas%*%I1)^(-1))%*%t(x_test)%*%y_test
}

ridge_regr_test()
pred_test_ridge <- x_test%*%(((t(x_test)%*%x_test+alphas%*%I1))^(-1))%*%t(x_test)%*%y_test
compare1 <- matrix(c(trainingData$median_house_value, pred_test_ridge), ncol = 2)
compare1

#ridge regression on x_train
alphas <- matrix(seq(0.0, 1.0, by = .1), ncol = 8, nrow = 8)
I2 <- diag(8)

ridge_regr_train <- function () {
  ((t(x_train)%*%x_train+alphas%*%I2)^(-1))%*%t(x_train)%*%y_train
}

ridge_regr_train()
pred_train_ridge <- x_train%*%(((t(x_train)%*%x_train+alphas%*%I2))^(-1))%*%t(x_train)%*%y_train
compare2 <- matrix(c(testData$median_house_value, pred_train_ridge), ncol = 2)
compare2


#evaluations using notations of ridge regression
evaluations <- function (true, predicted, df) {
  ssr <- sum((pred_ridge - mean(y))^(2))
  sse <- sum((pred_ridge - y)^2)
  sst <- ssr + sse
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

