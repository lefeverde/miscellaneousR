limma_voom_longtable_maker <- function(dgelist_object, design){
 #create a longtable of DEG FC output
 
 fit <- eBayes(lmFit(voom(dgelist_object), design))
 out_df <- data.frame()
 for(i in colnames(design)){
   if(!grepl('Intercept', i)){
     cur_table <- topTable(fit,
                           coef = i,
                           number = Inf, 
                           genelist = fit$genes$gene_name,
                           confint = TRUE,
                           adjust='fdr')
     cur_table <- data.frame(group=i,gene_id=row.names(cur_table), cur_table)
     out_df <- rbind(out_df,cur_table)
   }
 }
 # fixing column ordering and names
 gene_names <- out_df$ID
 group <- out_df$group
 out_df <- data.frame(gene_id=out_df$gene_id,
                      gene_names=gene_names,
                      group=group,
                      out_df[,3:length(out_df)],
                      row.names=NULL)
 return(out_df)
  
}