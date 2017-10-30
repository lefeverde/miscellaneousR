base_volc_plotter_temp <- function(plot_data,
                                   plot_labels=TRUE,
                                   label_num=35
                                   
                                   
                                 
                             
){
  library(ggplot2)
  library("ggrepel") #Avoid overlapping labels
  
  # Third worst hack I have had to do get the goddamn plot 
  # to work. Basically, ggplot was misalinging the 0 label
  # and to fix it, I converted the nums to string and 
  # replaced the zero with a '0' with a space
  # The x_labels are to create a geom_segment 
  # that does not span the entire plot lenght. It would
  # retarded to have the ends of that under the zero.
  
  gd_y_text <- as.character(pretty(-log10(plot_data$padj), n=5))
  gd_y_text[1] <- '0 '

  plot_title = gsub('_', ' ',levels(factor(plot_data$group)))
  gd_x_breaks <- pretty(plot_data$log2FoldChange, n=7) 
  gd_y_breaks <- pretty(-log10(plot_data$padj), n=5)
  plt1 <- ggplot(data=plot_data,
                 aes(x=log2FoldChange,
                     y=-log10(padj))) +
    theme_bw() +
    
    geom_point(size=rel(.75),alpha=.5,colour='gray') +
    
    #scale_size_manual(values=c(rel(1.25), rel(.75)), guide=FALSE) +
    # Another stupid hack to shorten the sig line 
    
    geom_segment(aes(x= .75*min(gd_x_breaks), # horizontal line hack
                     xend= .75*max(gd_x_breaks),
                     y=1.30103,
                     yend=1.30103),
                 colour='#999999',
                 size=rel(.25), 
                 linetype='dashed',
                 alpha=.25) +
    # Next 2 geom_segs are for -1.3, 1.3 log2FC areas
    geom_segment(aes(x= 1.3, 
                     xend= 1.3,
                     y=0,
                     yend=.9*max(gd_y_breaks)),
                 colour='#999999',
                 size=rel(.25), 
                 linetype='dashed',
                 alpha=.25) +
    geom_segment(aes(x=- 1.3, 
                     xend= -1.3,
                     y=0,
                     yend=.9*max(gd_y_breaks)),
                 colour='#999999', 
                 size=rel(.25),
                 linetype='dashed',
                 alpha=.25) +

    scale_x_continuous(breaks = pretty(plot_data$log2FoldChange, n=7)) +
    scale_y_continuous(breaks = gd_y_breaks, labels = gd_y_text )
  
  plt2 <- plt1  + theme(plot.margin = unit(c(.1,.1,.1,.1), "cm"),
                        panel.background = element_blank(),
                       
                        axis.ticks = element_blank(), 
                        axis.title.y=element_text(size=rel(1.5), 
                                                  face="bold",
                                                  colour='#666666',
                                                  margin=margin(0,-9.25,0,0)),
                        axis.title.x=element_text(size=rel(1.5), 
                                                  face="bold",
                                                  colour='#666666',
                                                  margin=margin(2.25,0,0,0)),
                        axis.text.y=element_text(size=rel(1.5),
                                                 colour="#999999",
                                                 hjust=2.5), 
                        axis.text.x=element_text(size=rel(1.5), 
                                                 colour="#999999",
                                                 vjust=.5,
                                                 margin=margin(-5.0,0,0,0)
                                                 ),
                        legend.title=element_blank(),
                        legend.key = element_blank(),
                        legend.text=element_text(size=rel(1)),
                        legend.position = 'none',
                        panel.border=element_rect(fill=NA,
                                                  colour=NA,
                                                  size=rel(1)),
                        plot.title = element_blank()
                        )
  plt3 <- plt2 + labs(x='Log2 Fold Change',
                      y='-Log10 FDR p-value',
                      title=plot_title) 
  
  return(plt3)
}