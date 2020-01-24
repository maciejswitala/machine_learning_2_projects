conditional_density = function(data,i){
  
  title = colnames(data)[i]
  
  if(nchar(title)>24){
    title = substr(title, 1, 24)
    title = as.character(paste(title,"..."))
  }
  
  g <- ggplot(data, aes(data[,i]))
  g <- g + geom_density(aes(fill=data[,1]), 
                        alpha=0.8) + 
    ggtitle(title) + 
    labs(x="",y="") +
    theme(legend.title = element_blank()) +
    scale_fill_brewer(palette="Pastel1")
  
  return(g)
}