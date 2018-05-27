mallows <-
function (y, x, ndigit=3,  return.result=FALSE) 
{
    if (!is.vector(y)) return("First Argument has to be Numeric Vector") 
    m <- ncol(x)
    xname<-deparse(substitute(x))
    varNames<-c(deparse(substitute(y)),colnames(x))
    dta <- data.frame(x = x, y = y)
    mall<-leaps(x,y, method="Cp",nbest=1)
    out <- matrix(0,m,2+m)
    rownames(out) <- rep("",nrow(out))
    colnames(out) <- c("Number of Variables","Cp",colnames(x))
    for(i in 1:m) {
        out[i, ] <- c(i, round(mall$Cp[i],2), ifelse(mall$which[i,],"X",""))
    }
    if(return.result) return(out)
    print(out,quote=FALSE)

}
