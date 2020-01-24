
#sets proper directory
setwd("C:\\Users\\Maciek\\Desktop\\ML project")

source("XGBOOST_13.01.R")

#creates an explainer
#source: https://github.com/pbiecek/DALEX_docs/blob/master/vignettes/DALEX_and_xgboost.Rmd
predict_logit <- function(model, x) {
  raw_x <- predict(model, x)
  exp(raw_x)/(1 + exp(raw_x))
}

logit <- function(x) exp(x)/(1+exp(x))

explainer_xgb <- explain(xgboost_model1,
                         data = train_matrix, 
                         y = train_data$IsBadBuy, 
                         predict_function = predict_logit,
                         link = logit,
                         label = "xgboost")
explainer_xgb


#feature importance
set.seed(123)
vd_xgb <- variable_importance(explainer_xgb, type = "raw")

head(vd_xgb)
plot(vd_xgb)


#ALE plots
xgb_ale1 <- variable_response(explainer_xgb,
                              variable = "VehBCost",
                              type = "ale")

head(xgb_ale1)
plot(xgb_ale1)

xgb_ale2 <- variable_response(explainer_xgb,
                              variable = "MMRAcquisitonRetailCleanPrice",
                              type = "ale")

head(xgb_ale2)
plot(xgb_ale2)

xgb_ale3 <- variable_response(explainer_xgb,
                              variable = "VehicleAge",
                              type = "ale")

head(xgb_ale3)
plot(xgb_ale3)

xgb_ale4 <- variable_response(explainer_xgb,
                              variable = "Month",
                              type = "ale")

head(xgb_ale4)
plot(xgb_ale4)

