all_coefs_fc_fdr_tablemaker <- function(deseq_object, contrast_mat){
  # Creating empty df to put all the log2FC stuff
  
  temp_df <- data.frame()
 
  annot_data <- data.frame(gene_id=mcols(deseq_object)$gene_id,
                           gene_name=mcols(deseq_object)$gene_name)
  annot_data <- annot_data[order(annot_data$gene_id),]
 
  # for(i in resultsNames(deseq_object)){
  #   if(i != 'Intercept'){
  #     cur_res <- results(deseq_object, name=i, tidy = TRUE)
  #     names(cur_res)[1] <- 'gene_id'
  #     cur_res <- cur_res[order(cur_res$gene_id),]
  #     cur_res$group <- i
  #     cur_res$gene_name <- annot_data$gene_name
  #     temp_df <- rbind(temp_df, cur_res)
  #   }
  # }
  for(i in contrast_mat){
    cur_res <- results(deseq_object, contrast = i, tidy = TRUE)
    names(cur_res)[1] <- 'gene_id'
    cur_res <- cur_res[order(cur_res$gene_id),]
    group_name <- paste0(i[2], '_vs_', i[3])
    cur_res$group <- group_name
    cur_res$gene_name <- annot_data$gene_name
    temp_df <- rbind(temp_df, cur_res)
  }
  # ugly hardcoding to output a sesnesibly re-arranged df
  out_df <- data.frame(gene_id=temp_df$gene_id,
                       gene_name=temp_df$gene_name,
                       group=temp_df$group,
                       log2FoldChange=temp_df$log2FoldChange,
                       lfcSE=temp_df$lfcSE,
                       pvalue=temp_df$pvalue,
                       padj=temp_df$padj)
  return(out_df)
}