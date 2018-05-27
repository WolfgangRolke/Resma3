mlr <-
function (y, x, show.tests=FALSE,ndigit=3,  
          return.model=FALSE, return.result=FALSE) 
{
    if (!is.vector(y)) return("First Argument has to be Numeric Vector") 
    m <- ncol(x)
    xname<-deparse(substitute(x))
    varNames<-c(deparse(substitute(y)),colnames(x))
    dta <- data.frame(x = x, y = y)
    fit<-lm(y~.,data=dta)
    if(return.model) return(fit)
    txt1 <- "The least squares regression equation is:"
    const <- round(coef(fit)[1],ndigit)    
    preds <- round(coef(fit)[-1],ndigit)    
    txt1[2]<-paste(varNames[1]," = ",const)
    for(i in 1:m) {
        sgn <- ifelse(preds[i]>0,"+","-")   
        txt1[2]<-paste(txt1[2], sgn,abs(preds[i]),varNames[i+1])
    }     
    if(return.result) {
         cf<-c(const,preds)
         names(cf)<-c("Intercept",varNames[-1])
         return(cf)
    }        
    cat(txt1[1],"\n",txt1[2],"\n")
    dta1<-data.frame(x=fitted(fit),y=residuals(fit))
    plt1<-ggplot(aes(x, y), data = dta1) + 
                geom_point() + 
                xlab("Fitted Values") + ylab("Residuals")+
                geom_hline(yintercept=0,colour="blue")
    plt2<-ggQQ(fit)
    multiple.graphs(plt1, plt2)
    pvals <- round(summary(fit)[[4]][,4],4)
    pvals <-ifelse(pvals<0.001,"0.000",pvals)
    t.tests <- data.frame(rep("",m+1),rep(0,m+1))
    colnames(t.tests) <- c("Variable","p value")
    t.tests[1,1] <- "Constant"
    t.tests[1,2] <- pvals[1]   
    for(i in 1:m) {
         t.tests[i+1,1] <- varNames[i+1]
         t.tests[i+1,2] <- pvals[i+1]
    }  
    txtR2<-paste("R^2 = ",round(100*unlist(summary(fit))$r.squared,1),"%",sep="") 
    if(show.tests) print(t.tests, row.names=F)    
    cat(txtR2,"\n")

}
