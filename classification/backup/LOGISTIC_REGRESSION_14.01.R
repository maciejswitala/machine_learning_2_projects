#changes IsBadBuy's levels
levels(data_final$IsBadBuy) <- c("No","Yes")

#one-hot-encoding (creating dummies)
data_final_ohe <- as.data.frame(one_hot(as.data.table(data_final)))
data_final_ohe <- data_final_ohe[,-1]

colnames(data_final_ohe)

#divides data set into training and testing sample 
set.seed(372532)
training_obs <- createDataPartition(data_final_ohe$IsBadBuy_Yes, 
                                    p = 0.6, 
                                    list = FALSE) 
train_data <- data_final_ohe[training_obs,]
test_data  <- data_final_ohe[-training_obs,]

#the formula of the model
model_formula <- IsBadBuy_Yes ~ 
  
  #removing Auction_OTHER (collinearity)
  Auction_ADESA + Auction_MANHEIM +
  
  #removing VehYear (strong correlation with VehicleAge)
  VehicleAge + 
  
  #removing Make_other (collinearity)
  Make_CHEVROLET + Make_CHRYSLER + 
  Make_DODGE + Make_FORD + Make_HYUNDAI + Make_JEEP + Make_KIA + 
  Make_NISSAN + Make_PONTIAC + 
  Make_SATURN + Make_SUZUKI + 
  
  #removing Model_other (collinearity)
  Model_1500_RAM_PICKUP_2WD + Model_CALIBER + Model_CARAVAN_GRAND_FWD_V6 +
  Model_COBALT + Model_IMPALA + Model_MALIBU_4C + Model_PT_CRUISER + 
  Model_PT_CRUISER_2_4L_I4_S + Model_SEBRING_4C + Model_TAURUS + Model_TAURUS_3_0L_V6_EFI +
  
  #removing Trim_other (collinearity)
  Trim_Bas + Trim_EX + 
  Trim_LS + Trim_LT + Trim_LX + Trim_SE + 
  Trim_SEL + Trim_SXT + Trim_Tou + Trim_XLT + 
  
  #removing SubModel_other (collinearity)
  SubModel_other + SubModel_2D_COUPE +
  SubModel_4D_SEDAN + SubModel_4D_SEDAN_EX +
  SubModel_4D_SEDAN_LS + SubModel_4D_SEDAN_LT + 
  SubModel_4D_SEDAN_LX + SubModel_4D_SEDAN_SE + 
  SubModel_4D_SEDAN_SXT_FFV + SubModel_4D_SUV_4_2L_LS +
  SubModel_4D_WAGON + SubModel_MINIVAN_3_3L +
  
  #removing Color_other (collinearity)
  Color_BEIGE + Color_WHITE + 
  Color_BLACK + Color_BLUE + Color_GOLD + Color_GREEN + 
  Color_GREY + Color_MAROON + Color_RED + Color_SILVER +
  
  #removing Transmission_other (collinearity)
  Transmission_AUTO + Transmission_MANUAL + 
  
  #removing WheelTypeIDs (correlated with WheelTypes)
  #removing WheelType_Special (collinearity)
  WheelType_Alloy + WheelType_Covers +
  
  VehOdo + 
  
  #removing Nationality_other (collinearity)
  Nationality_AMERICAN + Nationality_OTHER_ASIAN + 
  Nationality_TOP_LINE_ASIAN + 
  
  #removing Size_other (collinearity)
  Size_CROSSOVER + Size_COMPACT +
  Size_LARGE + Size_LARGE_SUV + Size_LARGE_TRUCK + Size_MEDIUM + 
  Size_MEDIUM_SUV + Size_SMALL_SUV + Size_SMALL_TRUCK + Size_SPECIALTY + 
  Size_SPORTS + Size_VAN + 
  
  #removing all TopThreeAmericanName variables (strong correlation with Make variables)
  
  #removing following variables (strong correlation with
  #MMRAcquisitionAuctionAveragePrice):
  #MMRAcquisitionAuctionCleanPrice, MMRAcquisitionRetailAveragePrice,
  #MMRAcquisitonRetailCleanPrice, MMRCurrentAuctionAveragePrice,
  #MMRCurrentAuctionCleanPrice, MMRCurrentRetailAveragePrice,
  #MMRCurrentRetailCleanPrice
  MMRAcquisitionAuctionAveragePrice +  
  
  BYRNO + 
  
  #removing VNST_other (collinearity)
  VNST_AZ + VNST_CA + VNST_CO + 
  VNST_FL + VNST_GA + VNST_NC + VNST_OK +
  VNST_SC + VNST_TN + VNST_TX + VNST_VA + 
  
  VehBCost + 
  
  #removing IsOnlineSale_0 (collinearity)
  IsOnlineSale_1 + 
  
  WarrantyCost + 
  
  #removing Month_1 (collinearity)
  Month_2 + Month_3 + Month_4 + Month_5 + Month_6 + Month_7 + 
  Month_8 + Month_9 + Month_10 + Month_11 + Month_12



#LOGISTIC REGRESSION

#regression 1 (all the chosen variables)
logit1 <- lm(model_formula, data = train_data)
summary(logit1)

#TRAIN
probabilities_train <- logit1 %>% predict(train_data, type = "response")
predicted_classes_train <- ifelse(probabilities_train > 0.5, 1, 0)
roc_train <- roc(predictor = probabilities_train, response = train_data$IsBadBuy_Yes)

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
roc_train <- roc(predictor = probabilities_train, response = train_data$IsBadBuy_Yes)

table_train <- confusionMatrix(as.factor(predicted_classes_train),
                               as.factor(train_data$IsBadBuy_Yes))

#TEST
probabilities_test <- logit1 %>% predict(test_data, type = "response")
predicted_classes_test <- ifelse(probabilities_test > optim_threshold, 1, 0)
roc_test <- roc(predictor = probabilities_test, response = test_data$IsBadBuy_Yes)

table_test <- confusionMatrix(as.factor(predicted_classes_test),
                              as.factor(test_data$IsBadBuy_Yes))