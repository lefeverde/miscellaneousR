pca_plotter <- function(transformed_data, sample_map,leg_row_num=3, gene_num=Inf){
  library(ggplot2)
  library(matrixStats)
  # Filter out any samples not listed in sample_map
  cur_subset_mat <- data.frame(transformed_data)
  cur_subset_mat <- transformed_data[,colnames(transformed_data) 
                                     %in% 
                                       rownames(sample_map)]
  #return(cur_subset_mat)
  # Taken from DESeq2 plotPCA function
  # Calculates the row wise variance
  rv <- rowVars(cur_subset_mat)
  #return(rv)
  
  # select the gene_num genes by variance
  # the seq_len thing looks weird, but it was in DESeq2 function
  # so leaving it.
  select <- order(rv, decreasing=TRUE)[seq_len(min(gene_num, length(rv)))]
  #return(select)
  # perform a PCA on the data in assay(x) for the selected genes
  fnmt_pcomp <- prcomp(t((cur_subset_mat)[select,]))

  var_exp <- (fnmt_pcomp$sdev^2)/sum(fnmt_pcomp$sdev^2)
  
  # Puts first 3 pcs into df
  plot_data <- data.frame(pc1=fnmt_pcomp$x[,1],
                          pc2=fnmt_pcomp$x[,2],
                          pc3=fnmt_pcomp$x[,3])
  # sorting because paranoia 
  
  plot_data <- plot_data[sort(rownames(plot_data)),]
  # Getting PCs 
  plot_data <- data.frame(pc1=fnmt_pcomp$x[,1],
                          pc2=fnmt_pcomp$x[,2],
                          pc3=fnmt_pcomp$x[,3])
  # merges sample metadata into df by rowname
  plot_data <- merge(plot_data, sample_map, by=0)
  # gets rid of extraneous column
  rownames(plot_data) <- plot_data$Row.names 
  plot_data$Row.names <- NULL 
  # changes metadata to column name to 'group'
  colnames(plot_data)[4] <- 'group'
  #eturn(plot_data)
  # This just makes the labels for axises 
  axlab.1 <- paste("PC1 (", 
                   signif(var_exp[1]*100, digits=4),"%)", sep="")
  axlab.2 <- paste("PC2 (",
                   signif(var_exp[2]*100, digits=4), "%)", sep="")
  axlab.3 <- paste("PC3 (",
                   signif(var_exp[3]*100, digits=4), "%)", sep="")
  
  
  # And here comes the plot!
  plt1 <- ggplot(data=plot_data,
                 aes(x=pc1,
                     y=pc2,
                     fill=group,
                     colour=group, 
                     shape=group,
                     label=row.names(plot_data))) + 
    # The geom point aes specificies colouring by group
    # and changes point shape by group as well
    # geom_point(size = 2.5, 
    #            aes(shape=factor(group), colour=factor(group))) +
    geom_point(size = rel(2.5)) +
    geom_hline(yintercept=0) +
    geom_vline(xintercept=0) +
    # gets a pretty colour set
    stat_ellipse(alpha=.15, geom = "polygon") +
    scale_colour_brewer(palette="Set1") +
    scale_fill_brewer(palette="Set1") +
    labs(x=axlab.1, y=axlab.2) + theme_bw()
  
  # This all just setting the themes the way I like it
  
  plt2 <- plt1 + theme(plot.margin = unit(c(1,1,1,1), "cm"),
                       panel.background = element_blank(),
                       axis.title.y=element_text(size=rel(1.75), 
                                                 
                                                 
                                                 face="bold",
                                                 margin=margin(0,7.5,0,0)),
                       axis.title.x=element_text(size=rel(1.75), 
                                                
                                                
                                                 face="bold",
                                                 margin=margin(7.5,0,0,0)),
                       axis.text.y=element_text(size=rel(1.5),
                                                colour="black"), 
                       axis.text.x=element_text(size=rel(1.5), 
                                                colour="black"),
                       legend.title=element_blank(),
                       legend.key = element_blank(),
                       
                       #legend.spacing.x=unit(10.5, 'line'),
                       #legend.spacing.y=unit(10.5, 'line'),
                       
                       legend.text=element_text(size=rel(1.25)),
                       legend.position = 'bottom',
                       panel.border=element_rect(fill=NA,
                                                 colour="black",
                                                 size=1))
  #title=element_text(size=22, 
  #                    colour="black", 
  #                   face="bold", 
  #                  vjust=1.5))
  
  # this just splits the legend into two rows 
  # when there is more than 3 groups because of 
  # ugly formatting 
  # if(length(levels(factor(plot_data$group))) > 3){
  #   plt2 <- plt2 + guides(col=guide_legend(nrow = 2))
  # }
  group_num <- length(levels(factor(plot_data$group)))
  if (group_num > 6){
    plt2 <- plt2 + scale_shape_manual(values = seq(1,group_num))
  }
  plt2 <- plt2 + guides(col=guide_legend(nrow = leg_row_num)) #+ geom_text(position = 'jitter')

  return(plt2)
}