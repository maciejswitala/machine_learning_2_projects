
correlations<-function(data_final,variables,continuous=1){
  
  if(continuous==1){
    #CORRELATIONS: pearson for continuous variables
    M <- cor(data_final[,continuous_variables], method='pearson')
    corrplot(M, method='color', number.cex=0.75, tl.offset = 0.1, tl.cex=0.5)
  }
  
  if(continuous!=1){
    
    #goodman-kruskal lambdas for categorical variables
    data_final_recoded <- data_final[,categorical_variables]
    lambdas <- c()
    lambdas_df <- data.frame(matrix(NA, nrow = length(categorical_variables), 
                                    ncol = length(categorical_variables)))
    colnames(lambdas_df) <- categorical_variables
    rownames(lambdas_df) <- categorical_variables
    
    for(i in c(1:length(data_final[,categorical_variables]))){
      for(j in c(1:length(data_final[,categorical_variables]))){
        lambda <- Lambda(data_final_recoded[,i],data_final_recoded[,j])
        lambdas_df[i,j] <- lambda
      }
    }
    
    corrplot(as.matrix(lambdas_df), method='color', number.cex=0.75, tl.offset = 0.1, tl.cex=0.5)
  }
}