prop.ps <-
function (n, phat, pi.null, power, alpha=0.05, alternative="equal",E, 
    conf.level=95, return.result=FALSE, return.graph = FALSE) 
{
    if(!missing(E)) {
         alpha <- 1-conf.level/100
         if(missing(phat)) phat<-1/2
         n <- ceiling(qnorm(1-alpha/2)^2*phat*(1-phat)/E^2)
         names(n)<-"n"
         if(return.result) return(n)
         txt1 <- paste("Sample size required is ",n)
         return(txt1)

    }
    np<-1000
    fun <- function(x,n) {
        if(alternative=="equal") alpha <- alpha/2
        if(alternative=="equal") 
            y <- pbinom(qbinom(alpha, n, pi.null), n, x) + 
                1 - pbinom(qbinom(1-alpha, n, pi.null), n, x)
        if(alternative=="less")
            y <- pbinom(qbinom(alpha,n,pi.null),n,x)
        if(alternative=="greater")    
            y <- 1-pbinom(qbinom(1-alpha,n,pi.null),n,x)
        y
            
    }
    if(missing(power)| missing(phat)) {
        if(!missing(phat))  
            x <- seq(max(0.01,phat-6*sqrt(pi.null*(1-pi.null)/n)), 
               min(0.99,phat+6*sqrt(pi.null*(1-pi.null)/n)),length=np)
        else x <- seq(0.01, 0.99,length=np)   
        if(alternative=="less") x<-x[x<pi.null]
        if(alternative=="greater") x<-x[x>pi.null]

        y <- fun(x,n)
        if(!missing(phat)) {
            pw <- y[abs(phat-x)==min(abs(phat-x))][1]
            names(pw) <- "Power"
            pw <- round(pw*100,1)
            if(return.result) return(pw)
            x0 <- x[abs(phat-x)==min(abs(phat-x))][1]
            plt <- splot(100*y, x, add.line=3, label_y="Power", label_x=expression(pi), return.graph=TRUE) +
                 geom_point(aes(x,y),data=data.frame(x=x0, y=pw), size=3, colour="red")  
           if(return.graph) return(plt)
           else print(plt) 
           txt1 <- paste("Power of Test = ",pw,"%",sep="")                  
       }
       else  {
          k<-n*x[abs(y-power/100)==min(abs(y-power/100))]
          names(k)<-"num.success"
          if(return.result) return(k)
          if(alternative=="equal")
               txt1 <- paste("Number of Successes required for power: <",floor(k)," or >",ceiling(n-k))
          if(alternative=="less")
               txt1 <- paste("Number of Successes required for power: <",floor(k))
          if(alternative=="greater")
               txt1 <- paste("Number of Successes required for power: >",ceiling(n-k))
       }   
     }
     if(missing(n)) {
          steps <- c(1000,100,10,1)
          m<-0
          for(i in 1:4) {
              j<-0
              repeat {
                  j<-j+1
                  if(fun(phat,m+j*steps[i])>power/100) {
                      m<-m+(j-1)*steps[i]
                      break
                  }    
              }
         }
         n <- m+1
         names(n)<-"n"
         if(return.result) return(n)
         txt1 <- paste("Sample size required is ",n)
    }    
    txt1
}
