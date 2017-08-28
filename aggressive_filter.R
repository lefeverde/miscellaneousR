aggressive_filter <- function(dgelist_object, decile_limit=2){
  library(preprocessCore)
  library(edgeR)
  # Function to fiter via quantile thresholding of
  # lowly expressed genes
  
  # Remove rows with all zeros:
  dgelist_object <- dgelist_object[!rowSums(getCounts(dgelist_object))==0,]
  
  # Adjust to cpm and remove zeros again:
  total_cpm <- rowSums(cpm(dgelist_object))
  total_cpm <- total_cpm[!total_cpm==0]
  
  # get the quanitle vals for the summed cpm
  quantile_vals <- quantile(total_cpm, c(seq(0, 1, by=.1)))
  genes_to_keep <- names(total_cpm[!total_cpm < quantile_vals[decile_limit]])
  out_dgelist_object <- dgelist_object[genes_to_keep,]
  
}