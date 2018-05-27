chi.gof.test <-
function (x,p,return.result=FALSE) 
{
      m<-length(p)
      p<-p/sum(p)
      if(length(x)>m) {
          n<-length(x)
          x<-table(x)
      }
      else n<-sum(x)
      E <- n*p
      txt1 <- ifelse(min(E)<5,"Warning : some expected counts<5","")
      chi<-sum((x-E)^2/E)
      pval <- round(1-pchisq(chi,m-1),4)
      names(pval) <- "p value"
      if(return.result) return(pval)
      if(pval<0.001) pval<-"0.000"
      txt1[2]<-paste("p value of test p=",pval,sep="")
      if(min(E)<5) cat(txt1[1],"\n")
      cat(txt1[2],"\n")

}
