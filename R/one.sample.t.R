one.sample.t <-
function (y,shat, n, mu.null, alternative="not.equal", ndigit=1, conf.level=95, 
    return.result=FALSE, return.graph=FALSE, no.graph=FALSE) 
{
  hyp.info <- hyptest.helper("mu", alternative, ifelse(missing(mu.null), 0, mu.null))  
    if(length(y)>1) {
        n <- length(y)
        varNames <- deparse(substitute(y))
        xbar <- mean(y)
        shat <- sd(y)
        if(!return.result) {
            plt <- nplot(y, return.graph=TRUE)
            if(return.graph) return(plt)
            if(!no.graph) print(plt)
        }   
    }
    else {
        plt <- NULL
        xbar <- y 
    }
    if(missing(mu.null)) {
         if(hyp.info$alternative=="two.sided") 
            crit <- qt( 1-(100-conf.level)/200,n-1) 
         else crit <- qt( 1-(100-conf.level)/100,n-1) 
         ci <- round(xbar+c(-1,1)*crit*shat/sqrt(n),ndigit)
         names(ci) <- c("Low","High")
         if(return.result) return(ci)
         if(hyp.info$alternative=="two.sided") 
              txt1 <- paste0("A ",conf.level,"% confidence interval for the population mean is (",
                  ci[1],", ",ci[2],")")
         if(hyp.info$alternative=="less")          
                txt1 <- paste0("A ",conf.level,"% confidence upper bound for the population mean is ", ci[2])
         if(hyp.info$alternative=="greater")          
                txt1 <- paste0("A ",conf.level,"% confidence lower bound for the population mean is ", ci[1])         
    } 
    else {
          T <- sqrt(n)*abs(xbar-mu.null)/shat
          pval <- round(1-pt(T,n-1),4)
          names(pval) <- "p value" 
          if(hyp.info$alternative=="two.sided") 
                pval<-2*pval
          if(return.result) return(pval)
          if(pval<0.001) pval<-"0.000"
          txt1 <- paste(hyp.info$txt , pval) 
    }      
    cat(txt1,"\n")     
 
}
