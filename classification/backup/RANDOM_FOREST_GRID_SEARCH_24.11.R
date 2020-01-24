
candidate_ntree = seq(50,300,50)
candidate_mtry = seq(3,15,1)

folds <- 5
n <- nrow(train_data)

set.seed(372332)
splitfolds <- sample(1:folds, n, replace = TRUE)

max_AUC = 0

for(ntree in candidate_ntree){
  for(mtry in candidate_mtry){
        
    list_of_aucs = c()
        
    for(fold in c(1:folds)){
          
      print(paste(ntree,mtry,fold))
          
      train_set <- train_data[splitfolds != fold, ]
      valid_set <- train_data[splitfolds == fold, ]
          
      set.seed(372332)
      rf_model1 <- randomForest(model_formula,
                                data=train_set,
                                ntree=ntree,
                                mtry=mtry,
                                importance=TRUE
      )
          
          
      #TRAIN
      probabilities_train <- as.numeric(predict(rf_model1, train_set, type = "prob")[,2])
      predicted_classes_train <- ifelse(probabilities_train > 0.5, 'Yes', 'No')
      roc_train <- roc(predictor = probabilities_train, response = train_set$IsBadBuy)
          
      min_diff = 1
      optim_threshold = NA
      for(i in c(1:length(roc_train$sensitivities))){
        diff = abs(roc_train$sensitivities[i]-roc_train$specificities[i])
        if(diff < min_diff){
          min_diff = diff
          optim_threshold = roc_train$thresholds[i]
        }
      }
          
      predicted_classes_train <- ifelse(probabilities_train > optim_threshold, 'Yes', 'No')
      roc_train <- roc(predictor = probabilities_train, response = train_set$IsBadBuy)
          
      #TEST
      probabilities_test <- as.numeric(predict(rf_model1, valid_set, type = "prob")[,2])
      predicted_classes_test <- ifelse(probabilities_test > optim_threshold, 'Yes', 'No')
      roc_test <- roc(predictor = probabilities_test, response = valid_set$IsBadBuy)

      AUC = auc(roc_test)
      print(AUC)
          
      list_of_aucs = append(list_of_aucs, AUC)
          
    }
        
    mean_AUC = mean(list_of_aucs)
    print(mean_AUC)
        
    if(mean_AUC > max_AUC){
      max_AUC = mean_AUC
      optimal_parameters = list(
        ntree = ntree, mtry = mtry
      )
    }
        
  }
}

optimal_parameters
