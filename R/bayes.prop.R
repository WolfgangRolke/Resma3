bayes.prop <-
function(k, n, pi=function(p) {1}, conf.level = 95) {
    alpha <- 1-conf.level/100  

    post.dens <- function(p) {
      y <- rep(0, length(p))
      for(i in 1:length(p)) {
        y[i] <- dbinom(k, n, p[i])*pi(p[i])
      }     
      y/mx
    }
    mx <- 1
    mx <- integrate(post.dens, 0, 1)$value
    L <- 0
    H <- 1
    repeat {
      M <- ( L+H )/2
      if(integrate(post.dens, 0, M)$value > alpha/2) 
        H <- M
      else L <- M  
      if( H-L < 0.000001 ) break  
    }
    Low <- M
    L <- 0
    H <- 1
    repeat {
      M <- ( L+H )/2
      if(integrate(post.dens, M, 1)$value < alpha/2) 
        H <- M
      else L <- M  
      if( H-L < 0.000001 ) break  
    }
    High <- M

    round(c(Low, High, as.numeric(prop.test(k, n, conf.level = 1-alpha)$conf.int)),4)
  }
