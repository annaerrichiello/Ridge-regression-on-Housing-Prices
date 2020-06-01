test <- house_pc[,trainIndex_housepc ] %>% data.matrix() # test data
x_test_housepc <- test  %>% data.matrix()
y_test_housepc <- matrix(c(median_house_value), nrow = 4, ncol = 1)

alphas <- matrix(seq(0.0, 0.1, by = .1), ncol = 3, nrow = 3)
I <- diag(3)
pred_housepc_ <- x_test_housepc%*%(((t(x_test_housepc)%*%x_test_housepc+alphas%*%I))^(-1))%*%t(x_test_housepc)%*%y_test_housepc
show(pred_housepc_)
compare <- matrix(c(median_house_value, pred_housepc_), ncol = 2)
compare

evaluations <- function (true, predicted, df) {
  sse <- sum((pred_housepc_ - y_test_housepc)^2)
  ssr <- sum((pred_housepc_ - mean(y_test))^(2))
  sst <- ssr + sse
  rsq <- 1 - sse / sst
  RMSE = sqrt(sse/nrow(df))
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    rsq = rsq
    
  )
  
}

evaluations(y_test_housepc, pred_housepc_, house_pc)

