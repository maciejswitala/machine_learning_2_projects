
data_preparation<-function(data, NAs='?', colNAsShare=0.5,
                           rowNAsShare=0.25, mp=0.01, ml=15){
  
  #saves as data.frame
  data <- as.data.frame(data)
  
  #recoding missings from chosen sign to 'NA'
  data[data==NAs] = NA
  
  #finds columns with at least colNAsShare of missings
  cols_to_drop<-c()
  for(i in c(1:ncol(data))){
    if(sum(is.na(data[,i]))>colNAsShare*nrow(data)){
      cols_to_drop<-append(cols_to_drop,i)
    }
  }
  #colnames(data)[cols_to_drop]
  
  #drops columns with at least colNAsShare of missings
  if(length(cols_to_drop)!=0){
    data <- data[,-cols_to_drop]
    
    #info about dropping
    print(paste('Following columns were removed due to share of NAs above',
                colNAsShare,":",colnames(data)[cols_to_drop]))
  }
  
  #finds rows with at least rowNAsShare of missings
  rows_to_drop<-c()
  for(i in c(1:nrow(data))){
    if(sum(is.na(data[i,]))>rowNAsShare*ncol(data)){
      rows_to_drop<-append(rows_to_drop,i)
    }
  }
  #rows_to_drop
  
  #drops rows with at least rowNAsShare of missings
  if(length(rows_to_drop)!=0){
    data <- data[,-rows_to_drop]
    
    #info about dropping
    print(paste('Following rows were removed due to share of NAs above',
                rowNAsShare,":",rows_to_drop))
  }
  
  #initial data preparation
  source("initial_data_preparation_10.11.R")
  data_prepared<-initial_data_preparation(data)
  
  #source('manual_recoding_11.12.R')
  #data_prepared<-manual_recoding(data_prepared)
  
  
  print("HERE")
  
  #recodes characters into factors
  for(i in data_prepared$characters){
    data_prepared$dataset[,i]<-as.factor(data_prepared$dataset[,i])
  }
  
  #columns with characters
  if(length(data_prepared$characters!=0)){
    print(paste('Character columns:',colnames(data_prepared$dataset)[data_prepared$characters],
                'were recoded into factors'))
  }
  
  #loads function for recoding categorical variables
  source('recode_levels_27.11.R')
  
  #mapps factors' levels
  data_final <- recode_levels(ds = data_prepared$dataset, mp = mp, ml=ml)
  
  #info about mapping the levels
  print(paste("The factors' levels were mapped in a way that there is no variable with more than",
              ml,"levels and there is no level with share lower than",mp*100,"%"))
  
  #omits remaining NAs
  data_final <- na.omit(data_final)
  print("Remaining missings were ommited")
  
  #replaces spaces and dots in levels' names with "_"
  for(i in c(1:ncol(data_final))){
    if(class(data_final[,i])=="factor"){
      for(j in c(1:length(levels(data_final[,i])))){
        levels(data_final[,i])[j] <- gsub(" ",
                                          "_", 
                                          levels(data_final[,i])[j], 
                                          fixed = TRUE)
      }
      #print(colnames(data_final)[i])
      #print(levels(data_final[,i]))
    }
  }
  #data_final$enginetype[data_final$enginetype == 'other'] <- 'dohcv'
 # data_final[130, 15] = 'dohcv'
  return(data_final)
}
