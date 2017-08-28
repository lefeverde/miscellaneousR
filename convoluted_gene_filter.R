convoluted_gene_filter <- function(gene_counts,
                                   pheno_data,
                                   thresh=.5){
  library(plyr)
  library(reshape2)
  
  names(pheno_data) <- 'disease' # Kludge
  
  binary_counts <- gene_counts # Kludge
  # Setting any value > 0 to 1
  binary_counts[binary_counts > 0] <- 1
  binary_counts <- data.frame(gene_id=row.names(binary_counts), binary_counts)
  row.names(binary_counts) <- NULL
  binary_counts_melt <- melt(binary_counts, id.vars = c('gene_id'))
  rm(binary_counts)
  binary_counts_melt$group <- pheno_data$disease[match(binary_counts_melt$variable,
                                                       row.names(pheno_data))]
  
  # Summing up binary counts by group
  bin_sum <- ddply(binary_counts_melt, 
                   .(gene_id, group), 
                   summarise, 
                   binary_sum=sum(value))
  rm(binary_counts_melt)
  # Converting back to wide format
  bin_sum <- recast(bin_sum, gene_id ~ group)
  row.names(bin_sum) <- bin_sum$gene_id
  bin_sum$gene_id <- NULL
  
  
  
  
  # Getting max value to be used as divisor 
  # this is so that the counts are scaled 
  # to account for difference in sample size
  # this is somewhat of a kludge but works
  max_vals <- apply(bin_sum, 2, function(x) max(x))
  bin_sum <- data.frame(t(t(bin_sum)/max_vals))
  
  filt_genes <- data.frame(bin_sum[rowSums(bin_sum) >= thresh,] >= thresh) 
  filt_genes <- filt_genes[apply(filt_genes, 1, any),]
  
  # Returning filtered gene count matrix
  out_counts <- gene_counts[row.names(filt_genes),]
  return(out_counts)
 
  
}