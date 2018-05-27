t.ps <-
function (n, diff, sigma,power, alpha=0.05, alternative="two.sided",
          E, conf.level=95, return.result = FALSE, return.graph = FALSE, 
          no.Graph = FALSE) 
{
    if(!missing(E)) {
         alpha <- 1-conf.level/100
         n <- ceiling((qnorm(1-alpha/2)*sigma/E)^2)
         names(n)<-"n"
         if(return.result) return(n)
         txt1 <- paste("Sample size required is ", n)
         return(txt1)
         
    }
    np <- 1000
    fun <- function(x,n) {
        qn <- ifelse(alternative=="two.sided",qt(1 - alpha/2,n-1),qt(1 - alpha,n-1))
        if(alternative=="two.sided") 
            y <- 1 - (pt(qn - sqrt(n) * abs(x)/sigma,n-1) - 
                      pt(-qn - sqrt(n) * abs(x)/sigma,n-1))
        else
            y <- 1-pt(qn - sqrt(n) * abs(x)/sigma,n-1)     
        100*y
    }
    if(missing(power) | missing(diff)) {
        if(alternative=="less") x <- seq( - 6 * sigma/sqrt(n),0, length = np)
        if(alternative=="greater") x <- seq(0, 6 * sigma/sqrt(n), length = np)        
        if(alternative=="two.sided") 
            x <- seq(- 6 * sigma/sqrt(n),   6 * sigma/sqrt(n), length = np)        
        if(!missing(diff)) x <- sort(c(x,diff))    
        y <- fun(x,n)

        if(!missing(diff)) {
            pw <- y[x==diff][1]
            names(pw) <- "Power"
            pw <- round(pw, 1)
            if(return.result) return(pw)
            plt <- splot(y,x,add.line=3, label_y="Power", label_x="diff", return.graph = TRUE) + 
                geom_point(aes(x,y),data=data.frame(x=diff,y=pw),size=3,colour="red")
           if (return.graph) return(plt)
           else {
              if(!no.Graph) print(plt) 
           }   
           txt1 <- paste("Power of Test = ",pw,"%",sep="")    
       }
       else  {
          diff<-round(x[abs(y-power)==min(abs(y-power))][1],3)
          names(diff)<-"difference"
          txt1 <- paste("Difference needed for power of test: diff = ",diff,sep="")  
       }   

     }
     if(missing(n)) {
          steps <- c(1000,100,10,1)
          m<-0
          for(i in 1:4) {
              k<-0
              repeat {
                  k<-k+1
                  if(fun(diff,m+k*steps[i])>power) {
                      m<-m+(k-1)*steps[i]
                      break
                  }    
              }
         }
         n <- m+1
         names(n)<-"n"
         if(return.result) return(n)
         txt1 <- paste("Sample size required is ",n)
    }    
    cat(txt1,"\n")
    
}
