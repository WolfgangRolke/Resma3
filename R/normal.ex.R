normal.ex <-
function (k=1) 
{
      if(k==1) return(rt(100,2))
      if(k==2) return(rexp(100,2))
      if(k==3) return(rnorm(100,5))
      if(k==4) return(rchisq(100,2))
      if(k==5) return(rbeta(100,5,1))
      if(k==6) return(rnorm(100,100,10))
      if(k==7) return(rgamma(100,10,1))
      if(k==8) return(runif(100))
      if(k==9) return(rnorm(100))
      if(k==10) return(rt(100,1))
      NULL
}
