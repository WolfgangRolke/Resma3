oneway <-
function (y,x, ndigit=1,  var.equal = TRUE, conf.level=95, 
    return.result=FALSE, return.graph=FALSE, no.Graph=FALSE) 
{
    if (!is.vector(y)) return("First Argument has to be Numeric Vector") 
    if(missing(x))  return("Second Argument has to be categorical Vector") 
    varNames <- c(deparse(substitute(y)), deparse(substitute(x)))
    if (is.numeric(x)) x <- as.factor(x)
    dta <- data.frame(x = x, y = y)
    
    if(length(unique(x))==2) {
          xVals<-unique(x)
          fit <-t.test(y[x==xVals[1]],y[x==xVals[2]],conf.level = conf.level/100)
          ci <- round(as.numeric(unlist(fit)[c(4,5)]),ndigit)
          names(ci) <- c("Low","High")
          if(return.result) return(ci)
          txtCI <- paste("A ",conf.level,"% confidence interval for the difference in group means is (",
                      ci[1],", ",ci[2],")",sep="")
    } 
    else txtCI<-""
    if(var.equal) {
        fit<-aov(y~x,data=dta)       
        pval<-round(as.numeric(unlist(summary(fit))[9]),4)
        sds<-round(tapply(dta$y,dta$x,sd),ndigit)
        txt2 <- paste("Smallest sd: ",min(sds),"   Largest sd :",max(sds))
        plt<-ggQQ(fit)        
    }
    else {
        fit<-oneway.test(y~x,data=dta)
        pval <- round(as.numeric(unlist(fit)[4]),4)
        txt2 <- "No equal variance assumed, no normal plot of residuals!"
    }    
    names(pval)<- "p value"
    if(return.result) return(pval)
    if(pval<0.001) pval<-"0.000"
    txt1<-paste("p value of test of equal means: p =",pval)  
    if(return.graph) return(plt) 
    else if(!no.Graph) print(plt)
    cat(txt1,"\n")
    cat(txt2,"\n")
    if(length(unique(x))==2) cat(txtCI,"\n")

}
