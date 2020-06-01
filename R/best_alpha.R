temp <- data.frame()
for (i in 0:10) {
  fit.name <- paste(alpha=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0))
  
  ## Use each model to predict 'y' given the Testing dataset
  eval <- c(pred_test_ridge_0.0 <- x_test%*%(((t(x_test)%*%x_test+matrix(c(0.0), ncol=8,nrow=8)%*%I1))^(-1))%*%t(x_test)%*%y_test,
            pred_test_ridge_0.1 <- x_test%*%(((t(x_test)%*%x_test+matrix(c(0.1), ncol=8,nrow=8)%*%I1))^(-1))%*%t(x_test)%*%y_test,
            pred_test_ridge_0.2 <- x_test%*%(((t(x_test)%*%x_test+matrix(c(0.2), ncol=8,nrow=8)%*%I1))^(-1))%*%t(x_test)%*%y_test,
            pred_test_ridge_0.3 <- x_test%*%(((t(x_test)%*%x_test+matrix(c(0.3), ncol=8,nrow=8)%*%I1))^(-1))%*%t(x_test)%*%y_test,
            pred_test_ridge_0.4 <- x_test%*%(((t(x_test)%*%x_test+matrix(c(0.4), ncol=8,nrow=8)%*%I1))^(-1))%*%t(x_test)%*%y_test,
            pred_test_ridge_0.5 <- x_test%*%(((t(x_test)%*%x_test+matrix(c(0.5), ncol=8,nrow=8)%*%I1))^(-1))%*%t(x_test)%*%y_test,
            pred_test_ridge_0.6 <- x_test%*%(((t(x_test)%*%x_test+matrix(c(0.6), ncol=8,nrow=8)%*%I1))^(-1))%*%t(x_test)%*%y_test,
            pred_test_ridge_0.7 <- x_test%*%(((t(x_test)%*%x_test+matrix(c(0.7), ncol=8,nrow=8)%*%I1))^(-1))%*%t(x_test)%*%y_test,
            pred_test_ridge_0.8 <- x_test%*%(((t(x_test)%*%x_test+matrix(c(0.8), ncol=8,nrow=8)%*%I1))^(-1))%*%t(x_test)%*%y_test,
            pred_test_ridge_0.9 <- x_test%*%(((t(x_test)%*%x_test+matrix(c(0.9), ncol=8,nrow=8)%*%I1))^(-1))%*%t(x_test)%*%y_test,
            pred_test_ridge_1.0 <- x_test%*%(((t(x_test)%*%x_test+matrix(c(1.0), ncol=8,nrow=8)%*%I1))^(-1))%*%t(x_test)%*%y_test)

  ## Calculate the Mean Squared Error
  mse <- c(mse_0.0 <- mean((y_test-pred_test_ridge_0.0)^(2)),mse_0.1 <- mean((y_test-pred_test_ridge_0.1)^(2)),
    mse_0.2 <- mean((y_test-pred_test_ridge_0.2)^(2)),mse_0.3 <- mean((y_test-pred_test_ridge_0.3)^(2)),
    mse_0.4 <- mean((y_test-pred_test_ridge_0.4)^(2)),mse_0.5 <- mean((y_test-pred_test_ridge_0.5)^(2)),
    mse_0.6 <- mean((y_test-pred_test_ridge_0.6)^(2)),mse_0.7 <- mean((y_test-pred_test_ridge_0.7)^(2)),
    mse_0.8 <- mean((y_test-pred_test_ridge_0.8)^(2)),mse_0.9 <- mean((y_test-pred_test_ridge_0.9)^(2)),
    mse_1.0 <- mean((y_test-pred_test_ridge_1.0)^(2))) %>% data.matrix()
  
  ## Store the results
  temp <- data.frame(mse=mse,alpha=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0))
}

view(temp)
plot(temp)

mse_min <- apply(mse,2,min)
mse_min #it corresponds to value of alpha equal to 1.0



