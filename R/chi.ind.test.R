chi.ind.test <-
function (x,return.result=FALSE) 
{
      options(warn=-1)
      res<-chisq.test(x)
      options(warn=1)
      pval<-res$p.value
      names(pval) <- "p value"
      if(return.result) return(pval)
      E<-res$expected
      txt1<-ifelse(min(E)<5,"Some expected counts < 5!","")
      if(pval<0.001) pval<-"0.000"
      txt1[2]<-paste("p value of test p=",pval,sep="")
      if(min(E)<5) cat(txt1[1],"\n")
      cat(txt1[2],"\n")

}
