test.mean.sim <-
function (n=100, mu=50, mu.null=mu, sigma=25,alpha=0.05) 
{
    B<-10000
    pval<-rep(0,B) 
    for(i in 1:B) pval[i ] <- one.sample.t(rnorm(n, mu, sigma), mu.null=mu.null, return.result=TRUE) 
    plt <- hplot(pval,return.graph=TRUE)
    print(plt) 
    if(mu==mu.null) {
        txt1 <- paste("Nominal alpha: ",alpha)  
        txt1[2] <- paste("True alpha: ",round((sum(pval<alpha)/B),4))
        cat(txt1[1],"\n")
        cat(txt1[2],"\n")
    }
    else {
        txt1 <- paste("Power of Test: ",round((sum(pval<alpha)/B*100),4),"%",sep="")
        cat(txt1,"\n")
    }

}
