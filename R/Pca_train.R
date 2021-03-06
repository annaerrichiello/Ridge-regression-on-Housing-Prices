trainIndex_housepc <- sample(nrow(house_pc), 0.90*nrow(house_pc)) # indices for 90% 
train <- house_pc[trainIndex_housepc, ]
x_train_housepc <- train %>% data.matrix()
y_train_housepc <- matrix(c(median_house_value))

alphas <- matrix(seq(0.0, 1.0, by = .1), ncol = 8, nrow = 8)
I <- diag(8)
pred_housepc <- x_train_housepc%*%(((t(x_train_housepc)%*%x_train_housepc+alphas%*%I))^(-1))%*%t(x_train_housepc)%*%y_train_housepc




evaluations <- function (true, predicted, df) {
  ssr <- sum((pred_housepc - mean(y_train_housepc))^(2))
  sse <- sum((pred_housepc - y_train_housepc)^2)
   sst <- ssr + sse
   rsq <- 1 - sse / sst
  RMSE = sqrt(sse/nrow(df))
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    rsq = sprintf("%.8f", rsq)
  )
  
}

evaluations(y_train_housepc, pred_housepc, train)

