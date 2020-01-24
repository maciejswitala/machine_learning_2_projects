factors_levels = function(data, i){
  
  title = colnames(data)[i]
  
  if(nchar(title)>24){
    title = substr(title, 1, 24)
    title = as.character(paste(title,"..."))
  }
  
  var <- na.omit(data[,i])
  nrows <- 10 #10 bars to make 1 cell an equivalent of 1% of the column values
  df <- expand.grid(y = 1:nrows, x = 1:nrows)
  categ_table <- round(table(var) * ((nrows*nrows)/(length(var))))
  
  #if sum after rounding is not 100, correction needed
  #the biggest share is corrected (not much information is lost)
  if(sum(categ_table)>100){
    too_much<-sum(categ_table)-100
    categ_table[(as.numeric(which(categ_table==max(categ_table))))[1]]<-max(categ_table)-too_much
  }
  
  if(sum(categ_table)<100){
    not_enough<-100-sum(categ_table)
    categ_table[(as.numeric(which(categ_table==max(categ_table))))[1]]<-max(categ_table)+not_enough
  }
  
  df$category <- factor(rep(names(categ_table), categ_table))
  
  #waffle plot for factor's levels
  waffle_plot_1<-ggplot(df, aes(x = x, y = y, fill = category)) + 
    geom_tile(color = "black", size = 0.5) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
    scale_fill_brewer(palette = "Set3") +
    labs(title=title)
  
  return(waffle_plot_1)
}
