clt.illustration <-
function (k=1,p=0.5,n=1000,graphname) 
{
      y<-c(rnorm(p*n*k,10,3),rnorm((1-p)*n*k,30,3))
      x<-round(matrix(sample(y),n,k),1)
      if(k==1) {
          cat("x \n")
          for(i in 1:5) print(x[i,1])
          cat("...\n")
          for(i in (n-4):n) print(x[i,1])
      }  
      else {
          xbar<-apply(x,1,mean)
          cat("(");for(j in 1:(k-1)) cat("x",j," + ",sep="");cat("x",k,")/",k," = xbar\n",sep="")
          for(i in 1:5) {
              cat("(")
              for(j in 1:(k-1)) cat(x[i,j]," + ")
              cat(x[i,k],")/",k," = ",xbar[i]," \n")
          }    
          cat("...\n")
          for(i in (n-4):n) {
              cat("(")
              for(j in 1:(k-1)) cat(x[i,j]," + ")
              cat(x[i,k],")/",k," = ",xbar[i]," \n")
          }
          x<-xbar
      }  
      plt<-hplot(x,f="norm",par=c(mean(x),sd(x)),return.graph=T)
      if (!missing(graphname)) {
            file <- paste(localfolder, graphname, ".png", 
                sep = "")
            print(file)
            png(file)
            print(plt)
            dev.off()
     }
     else print(plt)
     
}
