fivenumber <-
function (y, x, ndigit=1, return.result=FALSE) 
{
    if(any(is.na(y))) {
        n <- length(y)
        I <- (1:n)[is.na(y)]
        numNA <- length(I)
        if(!missing(x)) x <- x[-I]
        y <- y[-I]
        cat("Warning: ", numNA, " missing values were removed!\n\n")    
    }
    if(missing(x)) {
        out <- cbind(min(y), quantile(y, 0.25), median(y), 
                quantile(y, 0.75), max(y))
        rownames(out) <- deparse(substitute(x))
    }    
    else {
        k <- length(unique(x))
        out <- cbind(tapply(y, change.order(x,1:k), min), 
            tapply(y, change.order(x,1:k), quantile, prob=0.25), 
            tapply(y, change.order(x,1:k), median), 
            tapply(y, change.order(x,1:k), quantile, prob=0.75), 
            tapply(y, change.order(x,1:k), max))
        rownames(out) <- unique(x)
    }
    colnames(out) <- c("Minimum","Q1","Median","Q3","Maximum")
      
    out <- round(out, ndigit)
    if(return.result) {
        if(missing(x)) z <- c(out, out[4]-out[2])
        else z <- cbind(out, IQR=out[,4]-out[,2])
        return(z)
    }    
    print(out)
    if(missing(x)) txt <- paste("IQR = ",out[4]-out[2])
    else txt <- paste("IQRs: ", paste(out[, 4]-out[, 2], collapse=","))
    cat(txt,"\n")

}
