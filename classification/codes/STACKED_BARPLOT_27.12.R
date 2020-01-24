stacked_barplot = function(data,i){
  
  title = colnames(data)[i]
  
  if(nchar(title)>24){
    title = substr(title, 1, 24)
    title = as.character(paste(title,"..."))
  }
  
  #prepares stacked barplot
  temp<-data
  temp$gr_var<-data[,i]
  temp$sukces<-data[,1]
  
  name<-colnames(data)[i]
  
  #prepares table with shares
  df_sum <- temp %>%
    group_by(gr_var, sukces) %>%
    summarise(Freq = sum(as.numeric(sukces)))
  
  df_sum1 <- df_sum %>%
    group_by(gr_var) %>%
    summarise(Total = sum(as.numeric(Freq)))
  
  df_sum <- merge(x=df_sum,y=df_sum1,by="gr_var")
  
  df_sum <- df_sum %>%
    mutate(Prop = Freq/Total)
  
  df_sum$sukces = as.factor(df_sum$sukces)
  df_sum$kicked = df_sum$sukces
  
  #barplot
  succ_dist<-ggplot(df_sum) +
    aes(x = gr_var, y = Freq, fill = kicked) +
    geom_col() + 
    geom_text(aes(label = percent(Prop)), position = position_stack(.5), size=3) +
    labs(title=title, 
         x="", y="") +
    scale_fill_brewer(palette="Pastel1")
  
  return(succ_dist)
}
