
manual_recoding <- function(data_prepared){
  data_prepared$dataset[,"PurchDate_1"] <- as.Date(data_prepared$dataset[,"PurchDate_1"],"%d.%m.%Y")
  data_prepared$dataset[,"Month"] <- month(data_prepared$dataset[,"PurchDate_1"])
  data_prepared$dataset[,"Month"] <- as.factor(data_prepared$dataset[,"Month"])
  
  #vehicle year and age are wrongly seen as factors, need to be recoded
  data_prepared$dataset[,"VehYear"] <- as.numeric(as.character(data_prepared$dataset[,"VehYear"]))
  data_prepared$dataset[,"VehicleAge"] <- as.numeric(as.character(data_prepared$dataset[,"VehicleAge"]))
  
  return(data_prepared)
}
