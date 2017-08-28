limma_voom_longtable_maker <- function(dgelist_object, design){
 #create a longtable of DEG FC output
 vtrans_object <- voom(dgelist_object)
 fit <- eBayes(lmFit(dgelist_object, design))
 for(i in colnames(design)){
   if(!grepl('Intercept', i)){
     topTable()
   }
 }
  
}