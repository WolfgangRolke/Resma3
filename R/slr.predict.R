slr.predict <-
function (y, x, newx=x, polydeg=1, interval, 
          conf.level=95, no.intercept=FALSE, ndigit=2) 
{
    if (!is.vector(y)) return("First Argument has to be Numeric Vector") 
    if(!is.vector(x)) return("Second Argument has to be Numeric Vector") 
    namResp <- deparse(substitute(y))
    namPred <- deparse(substitute(x))
    z<-newx
    if(polydeg>1) {
          X<-matrix(0,nrow=length(x),ncol=polydeg)
          for(i in 1:polydeg) X[,i]<-x^i
          x<-X
          newX<-matrix(0,nrow=length(newx),ncol=polydeg)
          for(i in 1:polydeg) newX[,i]<-newx^i
          newx<-newX
    }      
    if(no.intercept)  fit<-lm(y~x-1)
    else fit<-lm(y~x)
    if(missing(interval)) est <- predict(fit,newdata=list(x=newx))
    else {
        if(interval=="PI")
            a <- predict(fit,newdata=list(x=newx),interval = "prediction",level=conf.level/100)
        if(interval=="CI")
            a <- predict(fit,newdata=list(x=newx),interval = "confidence",level=conf.level/100)
        if(length(newx)==1) {
            est <- a[1]
            L <-a[2]
            U<-a[3]
        }
        else {
            est <- a[,1]
            L <-a[,2]
            U<-a[,3]        
        }        
    }    
    txt1 <- matrix(0,length(est), 2+ifelse(missing(interval),0,2))
    if(missing(interval)) colnames(txt1) <- c(namPred,"Fit")
    else colnames(txt1)<-c(namPred, "Fit", "Lower", "Upper")
    rownames(txt1) <- rep("",nrow(txt1))
    for(i in 1:length(z)) {
        txt1[i,1:2]<-c(z[i],round(est[i],ndigit)) 
        if(!missing(interval)) 
          txt1[i,3:4]<-c(round(L[i], ndigit), round(U[i], ndigit))  
    } 
    txt1

}
