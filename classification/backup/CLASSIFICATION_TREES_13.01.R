#CLASSIFICATION TREES

#divides data set into training and testing sample 
set.seed(372532)
training_obs <- createDataPartition(data_final$IsBadBuy,
                                    p = 0.6, 
                                    list = FALSE) 
train_data <- data_final[training_obs,]
test_data  <- data_final[-training_obs,]

#the formula of the model
model_formula <- IsBadBuy ~ .

#SINGLE CLASSIFICATION TREE (NOT PRUNED)
tree1 <- rpart(model_formula,
               data = train_data,
               method = "class",
               control=rpart.control(
                 minsplit = 870,
                 minbucket = 435,
                 cp = -1,
                 xcal=10,
                 maxdepth = 30
                 ))

#tree1
#rpart.plot(tree1)

#summary(tree1)
printcp(tree1)



#TRAIN
probabilities_train <- as.numeric(rpart.predict(tree1, train_data)[,2])
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

predicted_classes_train <- ifelse(probabilities_train > optim_threshold, "Yes", "No")
roc_train <- roc(predictor = probabilities_train, response = train_data$IsBadBuy)

table_train <- confusionMatrix(as.factor(predicted_classes_train),
                               train_data$IsBadBuy)

#TEST
probabilities_test <- as.numeric(rpart.predict(tree1, test_data)[,2])
predicted_classes_test <- ifelse(probabilities_test > optim_threshold, "Yes", "No")
roc_test <- roc(predictor = probabilities_test, response = test_data$IsBadBuy)

table_test <- confusionMatrix(as.factor(predicted_classes_test),
                              test_data$IsBadBuy)

jpeg('cm_dt_notPrunned.jpg')

fourfoldplot(table_test$table, color = c("#CC6666", "#99CC99"), std='all.max',
             conf.level = 0, margin = 1, main = "Confusion Matrix")

dev.off()

round(table_test$overall[1],2)
round(table_test$byClass[c(1,2)],2)

jpeg('roc_dt_notPrunned.jpg')

plot(roc_train, main="ROC curve (decision tree)", col="blue")
lines(roc_test, col="red")
legend(0.55,0.25,c(paste("TRAIN, AUC=",round(auc(roc_train),2)),
                   paste("TEST, AUC=",round(auc(roc_test),2))),
       fill=c("blue","red"))

dev.off()
