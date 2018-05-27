stat.table <-
function (y, x, Mean=TRUE, Sort=FALSE, ndigit=1) 
{
    yName <- deparse(substitute(y))
    numNA<-0
    if(any(is.na(y))) {
        n<-length(y)
        I<-(1:n)[is.na(y)]
        numNA<-length(I)
        if(!missing(x)) x<-x[-I]
        y<-y[-I]
        cat("Warning: ", numNA," missing values were removed!\n\n")    
    }
    if(missing(x)) {
        A <- matrix(0,1,3)
        rownames(A)<-yName
        if(Mean) {       
            colnames(A)<-c("Sample Size", "Mean", "Standard Deviation")
            A[1, ] <- c(length(y),mean(y),sd(y))
        }
        else {       
            colnames(A)<-c("Sample Size", "Median", "IQR")
            A[1, ] <- c(length(y),mean(y),sd(y))
        }   
    }
    else {
        k<-length(unique(x))
        xName <- deparse(substitute(x))
        if (is.numeric(x)) x <- as.factor(x)
        A<-matrix(0, k, 3)
        rownames(A) <- unique(x)
        if(Mean) {       
            colnames(A)<-c("Sample Size", "Mean", "Standard Deviation")
            A[, 1] <- tapply(y, change.order(x, 1:k), length)
            A[, 2] <- tapply(y, change.order(x, 1:k), mean, na.rm=TRUE)
            A[, 3] <- tapply(y, change.order(x, 1:k), sd, na.rm=TRUE)
        }
        else {
            colnames(A)<-c("Sample Size", "Median", "IQR")
            A[, 1] <- tapply(y, change.order(x, 1:k), length)
            A[, 2] <- tapply(y, change.order(x, 1:k), median, na.rm=TRUE)
            A[, 3] <- tapply(y, change.order(x, 1:k), IQR, na.rm=TRUE)
        }
        if(Sort) {
            A <- A[order(A[, 2]), ]
        }
    }
    round(A, ndigit)
    
}
