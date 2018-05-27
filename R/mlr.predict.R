mlr.predict <-
function (y, x, newx=x, interval, conf.level=95, ndigit=2) 
{
    if(is.vector(newx)) oneDim<-TRUE
    else oneDim<-FALSE
    if (!is.vector(y)) return("First Argument has to be Numeric Vector") 
    varNames <- c(deparse(substitute(y)), deparse(substitute(x)))
    xnames<-colnames(x)
    dta <-data.frame(y=y,x=x)
    fit<-lm(y~.,data=dta)
    if(oneDim) newdta <- data.frame(x = rbind(newx))
    else  newdta <- data.frame(x = newx)
    colnames(newdta)<-colnames(dta)[-1]
    if(missing(interval)) est <- predict(fit,newdata=newdta)
    else {
        if(interval=="PI")
            a <- predict(fit,newdata=newdta,interval = "prediction",level=conf.level/100)
        if(interval=="CI")
            a <- predict(fit,newdata=newdta,interval = "confidence",level=conf.level/100)
        if(oneDim) {
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
    if(oneDim) 
        newx<-rbind(newx)
    if(missing(interval))
        txt0 <- paste("> <em>mlr.predict(",varNames[1],",",varNames[2],"newx=  )</em><p>")
    else    
        txt0 <- paste("> <em>mlr.predict(",varNames[1],",",varNames[2],
                ",newx=  ,interval=\" ",interval,"\")</em><p>")          
    txt1 <- matrix(0,length(est),dim(newdta)[2]+1+ifelse(missing(interval),0,2))
    if(missing(interval)) colnames(txt1) <- c(xnames,"Fit")
    else colnames(txt1)<-c(xnames,"Fit","Lower","Upper")
    rownames(txt1)<-rep("",nrow(txt1))  
    for(i in 1:dim(newdta)[1]) {
        txt1[i,1:(1+dim(x)[2])]<-unlist(c(newdta[i,],round(est[i],ndigit)) )
        if(!missing(interval)) txt1[i,dim(x)[2]+c(2:3)]<-c(round(L[i],ndigit),round(U[i],ndigit))  
    } 
    txt1

}
