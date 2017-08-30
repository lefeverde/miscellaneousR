pretty_bar_chart <- function(xvar, 
                             yvar,
                             groups,
                             gene_num=50, 
                             error_dat=NA,
                             ylab='',
                             xlab='',
                             plot_title=''){
  library(ggplot2)
  library(gridExtra)
  library(grid)
  library(gtable)
  
  
  ## Function to make pretty top howevever many up/down regulated
  ## genes. 
  
  # Put data into df and give it a colour.
  data_frame <- data.frame(xvar=xvar, yvar=yvar, groups=groups, error_dat=error_dat)
  data_frame$barcolor <- ifelse(data_frame$yvar > 0, 'red', 'blue')
  
  # Caclulates the breaks for the Y axis so all plots are same
  y_break_point <- pretty(c(min(yvar)*1.05, max(yvar)*1.05), n=7)
  return(data_frame)
  # So this loop breaks up into groups, gets the top genes
  # and puts that all into one data frame which is run through
  # ggplot and the grob object is added to a list (plot_list).
  
  
  
  plot_list = list()
  #plot_dat <- data.frame()
  for(i in levels(factor(data_frame$group))){
    cur_split <- na.omit(data_frame)
    cur_split <- data_frame[data_frame$group == i,,drop=TRUE]
    top_split <- cur_split[order(cur_split$yvar, decreasing=TRUE),][1:gene_num,]
    bot_split <- cur_split[order(cur_split$yvar, decreasing=FALSE),][1:gene_num,]
    temp_df <- rbind(top_split, bot_split)
    temp_df <- temp_df[order(temp_df$yvar),]
    plot_dat <- temp_df # Kludge
    plt1 <- ggplot(data = plot_dat,
                   aes(x=reorder(xvar, yvar), y=yvar)) +
      geom_bar(stat = 'identity', fill=plot_dat$barcolor) +
      #coord_flip() + 
      theme_light() + 
      labs(title='', x=xlab, y=ylab) 
    
    
    plt2 <- plt1 + theme(plot.margin=unit(c(.5,.25,.25,.25),"cm"),
                         axis.text.y=element_text(size = rel(.75),
                                                  colour="black"),
                         axis.title.y=element_text(size = rel(.75),
                                                  colour="black"),
                         axis.text.x=element_text(size=rel(.75),
                                                  angle = 90,
                                                  colour="black"
                                                  ),
                         axis.title.x=element_text(size=rel(.75),
                                                   colour="black"),
                         plot.title = element_text(hjust = 0.5))
    
    plt3 <- plt2 + facet_wrap(~ groups) + theme(legend.position = 'none',
                                                strip.text=element_text(size=rel(1.75)),
                                                panel.spacing = unit(.01, "lines")) 
    plt3 <- plt3 +
      scale_y_continuous(breaks=y_break_point,
                         limits = c(min(y_break_point),
                                    max(y_break_point))) +
      labs(x=xlab, y=ylab)
    #plt3 <- plt3 + list(...)[[-1L]]
    if(!any(is.na(error_dat))){
      plt3 <- plt3 + geom_errorbar(data=plot_dat,
                                   aes(ymin=yvar-error_dat, ymax=yvar+error_dat))
    }
    
    plot_list[[i]] <- ggplotGrob(plt3)
    #return(plt3)
    #return(plot_dat)
    
  }
  #return(plot_list)
  grid_title <- textGrob(paste0(plot_title, '\nTop ', gene_num, ' Up/Down Regulated Genes'),
                         gp = gpar(fontface = "bold", cex = 1.5))
  grid_object <- grid.arrange(do.call(rbind.gtable,plot_list),top=grid_title)
  #grid_object <- grid.arrange(do.call(cbind.gtable,plot_list),top=grid_title)
  #return(grid.arrange(do.call(rbind.gtable,plot_list)))                 
  return(plot_list)
}