pretty_bars <- function(xvar,yvar, ylab='', xlab='', main=''){
  plot_dat <- data.frame(x=xvar, y=yvar)
  plot_dat <- plot_dat[order(plot_dat$y, decreasing = TRUE),]
  plot_dat$x <- factor(plot_dat$x, levels=plot_dat$x)
  plot_dat$barcolor <- as.factor(ifelse(plot_dat$y > 0, 'red', 'blue'))
  plt1 <- ggplot(data=plot_dat, aes(x=x, y=y, fill=barcolor)) + theme_minimal() + geom_bar(stat='identity') + scale_fill_manual(values = c('blue', 'red')) + theme(legend.position = 'none') + coord_flip()
  plt2 <- plt1 + theme(plot.margin=unit(c(.5,.25,.25,.25),"cm"),
                       axis.text.y=element_text(size = rel(.9),
                                                colour="black"),
                       axis.title.y=element_text(size = rel(1.05),
                                                 colour="black"),
                       axis.text.x=element_text(size=rel(.9),
                                                
                                                colour="black"
                       ),
                       axis.title.x=element_text(size=rel(1.05),
                                                 colour="black"),
                       plot.title = element_text(hjust = 0.5)) + labs(title=main, x=xlab, y=ylab) 
  
  return(plt2)
  #return(plot_dat)
}