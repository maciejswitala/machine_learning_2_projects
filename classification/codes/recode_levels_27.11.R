
recode_levels <- function (ds, mp = 0.01, ml = 12) {
  
  var_list = names(ds[sapply(ds, is.factor)])
  n <- nrow(ds)
  
  #keeps levels with more than 'mp' percent of cases
  for (i in var_list){
    if(class(ds[[i]])=="factor"){
      keep <- levels(ds[[i]])[table(ds[[i]]) > mp * n]
      levels(ds[[i]])[which(!levels(ds[[i]])%in%keep)] <- "other"
    }
  }
  
  #keeps top 'ml' levels
  for (i in var_list){
    if(class(ds[[i]])=="factor"){
      keep <- names(sort(table(ds[i]),decreasing=TRUE)[1:ml])
      levels(ds[[i]])[which(!levels(ds[[i]])%in%keep)] <- "other"
    }
  }
  
  return(ds)
}