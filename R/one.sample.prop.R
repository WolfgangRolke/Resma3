one.sample.prop <-
function (x, n, pi.null, alternative="not.equal", 
          conf.level=95, ndigits=3, return.result=FALSE) 
{
    hyp.info <- hyptest.helper("pi", alternative, ifelse(missing(pi.null), 0, pi.null))  
    if(missing(pi.null)) {
         ci <- round(as.numeric(prop.test(x,n,conf.level=conf.level/100, 
                alternative=hyp.info$alternative)$conf.int), ndigits)
         names(ci) <- c("Low", "High")
         if(return.result) return(ci)  
         if(hyp.info$alternative=="two.sided")         
                txt1 <- paste("A ",conf.level,"% confidence interval for the population proportion is (",
                  ci[1],", ",ci[2],")",sep="")
         if(hyp.info$alternative=="less")          
                txt1 <- paste("A ",conf.level,"% confidence upper bound for the population proportion is ",ci[2],sep="")
         if(hyp.info$alternative=="greater")          
                txt1 <- paste("A ",conf.level,"% confidence lower bound for the population proportion is ",ci[1],sep="")
    } 
    else {
          pval<-round(as.numeric(prop.test(x, n, p=pi.null, 
                  alternative=hyp.info$alternative)$p.value),4)
          names(pval)<-"p value"      
          if(return.result) return(pval) 
          if(pval<0.001) pval<-"0.000"
          txt1 <- paste(hyp.info$txt , pval)       
    }
    cat(txt1,"\n")

}
