one.sample.wilcoxon <-
function(y, med.null, ndigit=1, alternative="not.equal", 
         conf.level = 95, return.result=FALSE) {
  hyp.info <- hyptest.helper("median", alternative, 
                    ifelse(missing(med.null), 0, med.null))  
  if(missing(med.null)) {
    out <- round(as.numeric(unlist(wilcox.test(y, conf.int=TRUE, conf.level=conf.level/100))[7:8]), ndigit)
    txt1 <- paste0("A ", conf.level, "% confidence interval for the population 
                   median is (", out[1], ", ", out[2], ")") 
  }
  else {
    pval <- as.numeric(unlist(wilcox.test(y, mu=med.null, 
              alternative=hyp.info$alternative))[2])                
    names(pval) <- "p value"    
    if(pval<0.001) pval<-0     
    if(return.result) return(pval)
    pval <- round(pval, 4)
    if(pval<0.001) pval<-"0.000"
    txt1 <- paste(hyp.info$txt , pval)
  }  
  cat(txt1,"\n")
  
}
