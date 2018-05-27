gen.cont.table.data <-
function(n, A, B, tbl = FALSE, rho) {
  require(mvtnorm)
  k <- length(A)
  m <- length(B)
  dta <- rmvnorm(n, sigma = matrix(c(1,rho,rho,1),2,2))
  x <- dta[,1]
  y <- dta[,2]
  xr <- quantile(x, c(0:k)/k)
  yr <- quantile(y, c(0:m)/m)
  u <- rep(A[1], n)
  for(i in 2:k) u[x >= xr[i]] <- A[i]
  v <- rep(B[1], n)
  for(i in 2:m) v[y >= yr[i]] <- B[i]
  if(tbl) return(table(u, v))
  cbind( u, v )
}
