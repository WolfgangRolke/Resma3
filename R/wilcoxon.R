wilcoxon <-
function(y,mu.null, return.result=FALSE,alternative = "equal", conf.level=95) {
    if(missing(mu.null)) {
        out<-unlist(wilcox.test(y, conf.int=T,conf.level=conf.level/100))
        ci <- round(as.numeric(out[7:8]),2)
        names(ci) <- c("Low","High")
        if(return.result) return(ci)
        txt1 <- paste("A ",conf.level,"% confidence interval for the population median is (",
                  ci[1],", ",ci[2],")",sep="")
    }
    else {
        if(alternative == "equal") {
              out<-unlist(wilcox.test(y, mu.null=mu.null))
              sgn <- "<>"
        }      
        else {
              out<-unlist(wilcox.test(y, mu.null=mu.null, alternative=alternative))
              sgn <- ifelse(alternative=="less","<",">")          
        }     
        pval <- round(as.numeric(out[2]),4)
        names(pval) <- "p value"
        if(return.result) return(pval)
        if(pval<0.001) pval[1]<-"0.000"
        txt1 <- paste("p value of test H0: median=",mu.null," vs. Ha: median",sgn,mu.null," : ",pval,sep="")       
    }
    cat(txt1,"\n")

}
