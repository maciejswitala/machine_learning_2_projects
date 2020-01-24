#XGBOOST

#divides data set into training and testing sample 
set.seed(372532)
training_obs <- createDataPartition(data_final$IsBadBuy,
                                    p = 0.6,
                                    list = FALSE) 
train_data <- data_final[training_obs,]
test_data  <- data_final[-training_obs,]

for(i in c(1:ncol(train_data))){
  train_data[,i] = as.numeric(train_data[,i])
  test_data[,i] = as.numeric(test_data[,i])
}

train_data[,1] = train_data[,1] - 1
test_data[,1] = test_data[,1] - 1

train_matrix = as.matrix(train_data[,-1])
train_xgb = xgb.DMatrix(train_matrix, label = train_data$IsBadBuy)

test_matrix = as.matrix(test_data[,-1])
test_xgb = xgb.DMatrix(test_matrix, label = test_data$IsBadBuy)

watchlist <- list(train = train_xgb, eval = test_xgb)

param = list(
  max_depth = 5, eta = 0.05, gamma = 0.01, min_child_weight = 0.1,
  max_delta_step = 5, objective = "binary:logistic", eval_metric = "auc"
)

xgboost_model1 = xgb.train(param, train_xgb, nround = 155, watchlist = watchlist)

#TRAIN
probabilities_train <- predict(xgboost_model1, train_xgb)
predicted_classes_train <- ifelse(probabilities_train > 0.5, 1, 0)
roc_train <- roc(predictor = probabilities_train, response = train_data$IsBadBuy)

min_diff = 1
optim_threshold = NA
for(i in c(1:length(roc_train$sensitivities))){
  diff = abs(roc_train$sensitivities[i]-roc_train$specificities[i])
  if(diff < min_diff){
    min_diff = diff
    optim_threshold = roc_train$thresholds[i]
  }
}

predicted_classes_train <- ifelse(probabilities_train > optim_threshold, 1, 0)
roc_train <- roc(predictor = probabilities_train, response = train_data$IsBadBuy)

table_train <- confusionMatrix(as.factor(predicted_classes_train),
                               as.factor(train_data$IsBadBuy))

#TEST
probabilities_test <- predict(xgboost_model1, test_xgb)
predicted_classes_test <- ifelse(probabilities_test > optim_threshold, 1, 0)
roc_test <- roc(predictor = probabilities_test, response = test_data$IsBadBuy)

table_test <- confusionMatrix(as.factor(predicted_classes_test),
                              as.factor(test_data$IsBadBuy))

jpeg('cm_xgboost.jpg')

fourfoldplot(table_test$table, color = c("#CC6666", "#99CC99"), std='all.max',
             conf.level = 0, margin = 1, main = "Confusion Matrix")

dev.off()

jpeg('roc_xgboost.jpg')

plot(roc_train, main="ROC curve (xgboost)", col="blue")
lines(roc_test, col="red")
legend(0.6,0.25,c(paste("TRAIN, AUC=",round(auc(roc_train),2)),
                  paste("TEST, AUC=",round(auc(roc_test),2))),
       fill=c("blue","red"))

dev.off()

xgb.importance(model = xgboost_model1)
