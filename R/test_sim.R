test_sim <-
function(f_sim, alpha) {
  xy <- f_sim(alpha=alpha)
  print(xy);return(0)
  hist(xy[,1], 100, freq=F, main="", xlab="x")
  cc <- 1
  dfx <- function(x) exp(-x)/(1-alpha)*((1+x)^(1-alpha)-1)/cc
  cc <- integrate(dfx, 0, Inf)$value 
  curve(dfx, 0, max(xy[,1]), add=TRUE, lwd=2,  col="blue")  
  hist(xy[,2], 100, freq=F, main="", xlab="y",xlim=c(0,5))
  dfy <- function(x) exp(-x)/(1+x)^alpha/cc
  curve(dfy, 0, 20, add=TRUE, lwd=2,  col="blue")
}
