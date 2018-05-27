ci.mean.sim <-
function (n=50, mu=100, sigma=25,conf.level=95) 
{
    B<-10000
    alpha <- 1-conf.level/100
    crit <- qt(1-alpha/2, n-1)
    left<-rep(0,B) 
    right<-rep(0,B)
    for(i in 1:B) {
        x<-rnorm(n,mu,sigma)
        left[i]<-mean(x)-crit*sd(x)/sqrt(n)
        right[i]<-mean(x)+crit*sd(x)/sqrt(n)
    }
    txt1 <- paste("Nominal coverage: ",conf.level,"%",sep="")  
    txt1[2] <- paste("True coverage: ",round((1-(sum(left>mu)+sum(right<mu))/B)*100,1),"%",sep="")
    cat(txt1[1],"\n")
    cat(txt1[2],"\n")

}
