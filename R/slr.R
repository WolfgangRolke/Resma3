slr <-
function (y, x, no.intercept=FALSE, polydeg=1,
          show.tests=FALSE, ndigit=3,
          return.result=FALSE, return.model=FALSE, 
          return.graph=FALSE, RFplot=FALSE) 
{
    if (!is.vector(y)) return("First Argument has to be Numeric Vector") 
    if(!is.vector(x)) return("Second Argument has to be Numeric Vector") 
    namResp <- deparse(substitute(y))
    namPred <- deparse(substitute(x))
    if(polydeg>1) {
        no.intercept<-FALSE
        xmat<-matrix(0,length(y),polydeg)
        for(i in 1:polydeg) xmat[,i]<-x^i
        x<-xmat
    }    
    if(no.intercept)  fit<-lm(y~x-1)
    else fit<-lm(y~x)
    if(return.model) return(fit)
    
    txt1 <- "The least squares regression equation is:"
    if(no.intercept) {
        cf <- round(coef(fit),ndigit)
        names(cf)<-namPred
        sgn <- ifelse(cf>0," ","-")
        txt1[2]<-paste(namResp," =",sgn,abs(cf),namPred)
    }
    else {
        cf <- round(coef(fit)[1],ndigit)    
        cf[2] <- round(coef(fit)[2],ndigit)    
        names(cf)<-c("Intercept",namPred)
        sgn <- ifelse(cf[2]>0,"+","-")   
        txt1[2]<-paste(namResp," =",cf[1],sgn,abs(cf[2]),namPred)
        if(polydeg>1) {
            for(i in 2:polydeg) {
                cf[i+1] <- round(coef(fit)[i+1],ndigit)
                names(cf)[i+1]<-paste(namPred,"^",i,sep="")
                sgn <- ifelse(cf[i+1]>0,"+","-")   
                txt1[2]<-paste(txt1[2]," ",sgn,abs(cf[i+1]),namPred,"^",i,sep="")
            }
        }    
    }
    if(!return.result) 
       cat(txt1[1], "\n", txt1[2], "\n")
    dta1 <- data.frame(x=fitted(fit),y=residuals(fit))
    plt1 <- ggplot(aes(x, y), data = dta1) + 
        geom_point() + 
        xlab("Fitted Values") + ylab("Residuals")+
        geom_hline(yintercept=0,colour="blue")
    plt2 <- ggQQ(fit)
    if(return.graph) 
      return(list(plt1=plt1, plt2=plt2))
    if(RFplot) 
      print(plt1)
    else 
      multiple.graphs(plt1, plt2)
            
    if(no.intercept & polydeg==1) {
        pvals <- c(NA, round(summary(fit)[[4]][4], 4))
        t.tests <- ""
        t.tests[2] <- paste(namPred,": p = ", pvals[2])
    }    
    else {
        pvals <- round(summary(fit)[[4]][1:2, 4], 4)
        t.tests <- paste0("Constant: p = ", pvals[1])   
        t.tests[2] <- paste0(namPred," : p = ", pvals[2])
        if(polydeg>1) {
            for(i in 2:polydeg) {
                pvals[i+1] <- round(summary(fit)[[4]][i+1,4], 4)
                t.tests[i+1] <- paste0(namPred,"^",i," : p = ", pvals[i+1])                
            }
        }
    }  
    R2 <- round(100*unlist(summary(lm(y~x)))$r.squared,2)
    R2txt <- paste("R^2 = ", R2, "%",sep="") 
    if(return.result) return(c(cf, R2, pvals))          
    if(show.tests) for(i in 1:length(t.tests)) cat(t.tests[i],"\n")
    cat(R2txt,"\n")

}
