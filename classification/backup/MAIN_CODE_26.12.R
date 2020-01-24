########## INTRODUCTION ##########

#sets English language
Sys.setenv(LANG = "en")

#sets proper directory
setwd("C:\\Users\\Maciek\\Desktop\\ML project")

#installs all the needed packages
source("TECHNICALS_25.12.R")

#loads the dataset
data <- read_delim("kick2.csv",delim=';')
data[data=='?'] = NA

########## DATA PREPROCESSING ##########

#prepares dataset for further analysis
source("DATA_PREPARATION_26.12.R")
data_final <- data_preparation(data)

#removing not needed columns
data_final <- data_final[,-c(2,3)]

levels(data_final$IsBadBuy) <- c("No","Yes")

continuous_variables <- c(
  "VehOdo","MMRAcquisitionAuctionAveragePrice","MMRAcquisitionAuctionCleanPrice",
  "MMRAcquisitionRetailAveragePrice","MMRAcquisitonRetailCleanPrice",
  "MMRCurrentAuctionAveragePrice","MMRCurrentAuctionCleanPrice",
  "MMRCurrentRetailAveragePrice","MMRCurrentRetailCleanPrice",
  "BYRNO","VNZIP1","VehBCost","WarrantyCost","VehYear","VehicleAge"
)

categorical_variables <- c(
  "Auction","Make","Model","Trim","SubModel","Color","Transmission",
  "WheelTypeID","WheelType","Nationality","Size","TopThreeAmericanName","VNST","IsOnlineSale",
  "Month"
)

########## CORRELATIONS ##########

#correlations' analyses
source('correlations_11.12.R')

#pearson for continuous variables
correlations(data_final,continuous_variables,1)

#goodman/kruskal lambda for categorical variables
correlations(data_final,categorical_variables,0)
