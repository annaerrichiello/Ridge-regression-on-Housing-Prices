trainIndex_housepc <- sample(nrow(house_pc), 0.90*nrow(house_pc)) # indices for 90% 
train <- house_pc[trainIndex_housepc, ]
x_train_housepc <- train %>% data.matrix()
y_train_housepc <- matrix(c(median_house_value))

alphas <- matrix(seq(3, -2, by = -.1), ncol = 8, nrow = 8)
I <- diag(8)
pred_housepc <- x_train_housepc%*%(((t(x_train_housepc)%*%x_train_housepc+alphas%*%I))^(-1))%*%t(x_train_housepc)%*%y_train_housepc
show(pred_housepc)
compare <- matrix(c(median_house_value, pred_housepc), ncol = 2)
compare

evaluations <- function (true, predicted, df) {
  sse <- sum((pred_housepc - y_train_housepc)^2)
  sst <- sum((pred_housepc - mean(y_train_housepc))^2)
  rsq <- 1 - sse / sst
  RMSE = sqrt(sse/nrow(df))
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    rsq = rsq
    
    
  )
  
}

evaluations(y_train_housepc, pred_housepc, house_pc)

