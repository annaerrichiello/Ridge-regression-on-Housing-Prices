#linear regression model
#predict on testData
H1 <- x_test%*%(1/(t(x_test)%*%x_test))%*%t(x_test) #hat matrix
predictions_test <- H1%*%y_test
show(predictions_test)
compare <- matrix(c(testData$median_house_value, predictions_test), ncol = 2)
compare

