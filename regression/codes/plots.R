create_plot <- function(dataset){
  for (i in 1:ncol(dataset)){
    if (is.factor(dataset[,i])==T){
      p <- ggplot(dataset, aes(x=data_final[,i], y=price, color=dataset[,i])) +
        geom_boxplot() +
        labs(title=paste('Boxplot of', colnames(dataset)[i]), x=colnames(dataset)[i], y='price') +
        theme(legend.title = element_blank())
      print(p)
      c <- ggplot(dataset, aes(factor(dataset[,i]))) + geom_bar(fill='#F08080') +
        labs(title=paste(colnames(dataset)[i],'frequency'), x=colnames(dataset)[i]) +
        theme(legend.title = element_blank())
      print(c)
    }
    if ((is.numeric(dataset[,i])==T & colnames(dataset)[i]!='price')==T){
      # q <- ggplot(data_final, aes(data_final[,i])) +
      #  geom_density(color='seagreen4') +
      # labs(title=paste(colnames(data_final)[i],'denisty'), x=colnames(data_final)[i]) +
      #    theme(legend.title = element_blank())
      #print(q)
      w <- ggplot(dataset, aes(x=dataset[,i])) +
        geom_histogram(aes(y=..density..), colour="black", fill="white") +
        geom_density(alpha=.2, fill="#FFC300") + geom_vline(aes(xintercept=mean(dataset[,i])),
                                                              color="#070E24", linetype="dashed", size=1)+
        labs(title=paste(colnames(dataset)[i],'frequency'), x=colnames(dataset)[i]) +
        theme(legend.title = element_blank())   
      print(w)
    }
  }
}