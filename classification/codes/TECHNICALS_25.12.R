
#enables creating waffle plots
if(!require(waffle)){install.packages("waffle")}
library(waffle)

if(!require(rlist)){install.packages("rlist")}
library(rlist)

#enables dplyr structure of code
if(!require(dplyr)){install.packages("dplyr")}
library(dplyr)

#needed for some aesthetical reasons
if(!require(scales)){install.packages("scales")}
library(scales)

#enables html tables etc.
if(!require(pdp)){install.packages("pdp")}
library(pdp)

#same as above
if(!require(gridExtra)){install.packages("gridExtra")}
library(gridExtra)

#same as above
if(!require(grid)){install.packages("grid")}
library(grid)

#same as above
if(!require(lattice)){install.packages("lattice")}
library(lattice)

#tables in Rmarkdown
if(!require(kableExtra)){install.packages("kableExtra")}
library(kableExtra)

#enables operations on strings
if(!require(stringr)){install.packages("stringr")}
library(stringr)

#correlations
if(!require(corrplot)){install.packages("corrplot")}
library(corrplot)

#need for using read_delim function
if(!require(readr)){install.packages("readr")}
library(readr)

#needed for using text_grob function
if(!require(ggpubr)){install.packages("ggpubr")}
library(ggpubr)

#needed for using createDataPartition function
if(!require(caret)){install.packages("caret")}
library(caret)

#needed for using months function
if(!require(lubridate)){install.packages("lubridate")}
library(lubridate)

#needed obviously for corrplots
if(!require(corrplot)){install.packages("corrplot")}
library(corrplot)

#needed for using Goodman Kruskal lambda
if(!require(DescTools)){install.packages("DescTools")}
library(DescTools)

#needed for step-wise regression in logit
if(!require(MASS)){install.packages("MASS")}
library(MASS)

#needed for one-hot-encoding the factors
if(!require(mltools)){install.packages("mltools")}
library(mltools)

#needed for one-hot-encoding the factors
if(!require(data.table)){install.packages("data.table")}
library(data.table)

#needed for computing ROC curve
if(!require(pROC)){install.packages("pROC")}
library(pROC)

#needed for single decision tree's computation
if(!require(rpart)){install.packages("rpart")}
library(rpart)

if(!require(rpart.plot)){install.packages("rpart.plot")}
library(rpart.plot)

if(!require(rattle)){install.packages("rattle")}
library(rattle)

#needed for random forest modelling
if(!require(randomForest)){install.packages("randomForest")}
library(randomForest)

#needed for xgboost modelling
if(!require(xgboost)){install.packages("xgboost")}
library(xgboost)

#needed for resampling using SMOTE algorithm
if(!require(DMwR)){install.packages("DMwR")}
library(DMwR)

#needed for explainable machine learning
if(!require(DALEX)){install.packages("DALEX")}
library(DALEX)

if(!require(factorMerger)){install.packages("factorMerger")}
library(factorMerger)

if(!require(ALEPlot)){install.packages("ALEPlot")}
library(ALEPlot)
