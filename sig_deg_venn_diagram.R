sig_deg_venn_diagram <- function(long_table, 
                            pval_col='adj.P.Val',
                            gene_col='gene_id',
                            group_col='group',
                            plot_title=''){
  library(plyr)
  library(VennDiagram)
  sigsub <- long_table[long_table[[pval_col]] < .05,]
  #total_counts <- count(long_table, as.character(group_col))
  gene_list <- list()
  for(i in levels(factor(sigsub[[group_col]]))){
    gene_list[[i]] <- sigsub[[gene_col]][sigsub[[group_col]]==i]
    
  }
  #return(gene_list)
  gene_list <- calculate.overlap(gene_list)
  #return(calculate.overlap(gene_list))
  plot.new()
  #dev.off()
  
  #par(#mai=c(1,1,1,1)
      #oma=c(.1,.1,.1,.1),
  #    mar=c(4.1,4.1, 3.1, 4.1))
  title(main=plot_title, line=1)
  group_categories <- gsub('_','\n',levels(factor(sigsub[[group_col]])))
  draw.pairwise.venn(length(gene_list[[1]]), 
                     length(gene_list[[2]]),
                     length(gene_list[[3]]),
                     category = group_categories, 
                     cat.pos = c(180,90),
                     #ind=FALSE,
                     fill=c('#4daf4a', '#984ea3'),
                     ext.text = FALSE, 
                     margin = .1,
                     cex=.75,
                     cat.dist = c(-.4,.075))
  #return(gene_list)
}