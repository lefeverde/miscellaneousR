volcano_plotter2 <- function(plot_data,
                             plot_genes,
                             plot_labels=TRUE,
                             label_num=35
                             
                            
                         #x_var,
                         #y_var,
                         ){
  library(ggplot2)
  library("ggrepel") #Avoid overlapping labels
  

  plot_title = gsub('_', ' ',levels(factor(plot_data$group)))
  #plot_data$fill_fact <- ifelse((abs(plot_data$log2FoldChange)>= 1.3 & plot_data$padj < .05), 1, 0)
  #plot_data$fill_fact <- factor(1)
  plot_data$fill_fact <- factor(ifelse(plot_data$gene_id %in% plot_genes$gene_id, 0, 1))
  plot_data$gene_name <- factor(ifelse(plot_data$gene_id %in% plot_genes$gene_id, plot_data$gene_name, ''))
  plt1 <- ggplot(data=plot_data,
                 aes(x=log2FoldChange,
                     y=-log10(padj),
                     #label=gene_name,
                     alpha=fill_fact,
                     colour=fill_fact,
                     size=fill_fact
                     )) +
    scale_colour_manual(values=c('red', 'gray')) + 
    theme_bw() +
    #geom_jitter() +
    #geom_point(size=.75, alpha=.25) +
    geom_point() +
    scale_alpha_manual(values = c(1, .5), guide=FALSE) +
    scale_size_manual(values=c(rel(1.25), rel(.75)), guide=FALSE) +
    geom_hline(yintercept = 1.30103,
               #alpha=.1,
               colour='#1f78b4', size=rel(.75), linetype='dashed') + 
    geom_vline(xintercept = 1.3,
               #alpha=.1,
               colour='#1f78b4', size=rel(.75), linetype='dashed') +
    geom_vline(xintercept = -1.3,
               #alpha=.1,
               colour='#1f78b4', size=rel(.75), linetype='dashed') +
    scale_x_continuous(breaks = pretty(plot_data$log2FoldChange, n=7)) +
    scale_y_continuous(breaks = pretty(-log10(plot_data$padj), n=7))

  plt2 <- plt1  + theme(plot.margin = unit(c(1,1,1,1), "cm"),
                        panel.background = element_blank(),
                        #panel.grid.major = element_blank(), 
                        #panel.grid.minor=element_blank(), 
                        axis.title.y=element_text(size=rel(1.25), 
                                                  #vjust=1.25, 
                                                  #hjust=.25, 
                                                  face="bold",
                                                  margin=margin(0,7.5,0,0)),
                        axis.title.x=element_text(size=rel(1.25), 
                                                  #vjust=-.5,
                                                  #hjust=.25, 
                                                  face="bold",
                                                  margin=margin(7.5,0,0,0)),
                        axis.text.y=element_text(size=rel(1.25),
                                                 colour="black"), 
                        axis.text.x=element_text(size=rel(1.25), 
                                                 colour="black"),
                        legend.title=element_blank(),
                        legend.key = element_blank(),
                        legend.text=element_text(size=rel(1)),
                        legend.position = 'none',
                        panel.border=element_rect(fill=NA,
                                                  colour=NA,
                                                  size=rel(1)),
                        plot.title=element_text(size=rel(1.5),
                                           hjust=.5,
                                           colour="black",
                                           
                                           face="bold"))
  plt3 <- plt2 + labs(x='Log2 Fold Change',
                      y='-Log10 FDR p-value',
                      title=plot_title) 
  if(plot_labels){
    #top_genes <- plot_data[order( plot_data$padj, -abs(plot_data$log2FoldChange)),][1:50,]
    # Breaks genes into negative and positive log2fc 
    # This is so both pos/neg sides of volc get annots
    neg_genes <- plot_data[plot_data$log2FoldChange < 0,]
    neg_genes <- neg_genes[order(neg_genes$padj, -abs(neg_genes$log2FoldChange)),][1:label_num,]
    # same as above except for the pos genes
    pos_genes <- plot_data[plot_data$log2FoldChange > 0,]
    pos_genes <- pos_genes[order(pos_genes$padj, -abs(pos_genes$log2FoldChange)),][1:label_num,]
    top_genes <- rbind(neg_genes, pos_genes)
    # plt3 <- plt3 + geom_text_repel(data=top_genes,
    #                                aes(x=log2FoldChange,
    #                                    y=-log10(padj),
    #                                    label=gene_name),
    #                                size=rel(.95),
    #                                segment.size=rel(.05))
    return(plt3)
    
    plt3 <- plt3 + geom_text_repel(data=plot_genes,
                                   inherit.aes = FALSE,
                                   aes(x=log2FoldChange,
                                       y=-log10(padj),
                                       label=gene_name),
                                   size=rel(4),
                                   force = 10,
                                   segment.size=rel(.25))
    
    # plt3 <- plt3 + geom_point(data=plot_genes,
    #                           inherit.aes = FALSE,
    #                           size = 1,
    #                           aes(x=log2FoldChange,
    #                               y=-log10(padj)
    #                               ),
    #                           colour='red')
  }
  
  
  # plt4 <- plt3 + facet_wrap(~ treatment, nrow = 2) + theme(legend.position = 'none',
  #                                                          strip.text=element_text(size=20),
  #                                                          panel.spacing = unit(1.75, "lines"))
  return(plt3)
}