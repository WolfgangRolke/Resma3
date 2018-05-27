dlr.predict <-
function (y, x, z, newx=x, newz=z, additive=FALSE, 
          interval, conf.level=95, ndigit=2) 
{
    if (!is.vector(y)) return("First Argument has to be Numeric Vector") 
    varNames <- c(deparse(substitute(y)), deparse(substitute(x)), deparse(substitute(z)))
    
    fit<-dlr(y,x,z,additive=additive,return.model=TRUE)
    dta <- data.frame(x = newx, z=ifelse(newz==unique(z)[1],0,1))

    if(missing(interval)) est <- predict(fit,newdata=dta)
    else {
        if(interval=="PI")
            a <- predict(fit,newdata=dta,interval = "prediction",level=conf.level/100)
        if(interval=="CI")
            a <- predict(fit,newdata=dta,interval = "confidence",level=conf.level/100)
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
    txt1 <- data.frame(matrix(0,length(est),3+ifelse(missing(interval),0,2)))
    if(missing(interval)) colnames(txt1) <- c(varNames[2:3],"Fit")
    else colnames(txt1)<-c(varNames[2:3],"Fit","Lower","Upper")
    for(i in 1:length(newx)) {
        txt1[i,1:3]<-c(newx[i],newz[i],round(est[i],ndigit)) 
        if(!missing(interval)) txt1[i,4:5]<-c(round(L[i],ndigit),round(U[i],ndigit))  
    }    
    print(txt1)

}
