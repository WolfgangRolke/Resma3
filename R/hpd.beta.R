hpd.beta <-
function(cl = 95, alpha, beta, acc = 1/1e4) {
      z <- 1-cl/100
      dF <- function(x) dbeta(x, alpha, beta)
      pF <- function(x) pbeta(x, alpha, beta)
      qF <- function(p) qbeta(p, alpha, beta)
      fun <- function(x) dF(x) - dF( qF( pF(x)+1-z ) )
      L <- 0
      if(alpha == 1 & beta == 1) return(c(z, 1-z))
      if(alpha == 1) return( c(0, qF( pF(0) + 1 - z) ) )
      if(beta == 1) return( c( qF( pF(1) - 1 + z) , 1) )
      low <- 0
      high <- qF(z)
      repeat {
        mid <- (low+high)/2
        if(fun(mid) < 0) low <- mid
        else high <- mid
        if(high-low < acc) break
      }
      L <- mid
      if(L<2*acc) L <-0
      H <- qF( pF(L)+1-z )
      if(H>1-2*acc) H <-1
      curve(dF, qF(0.001), qF(0.999))
      abline(h=dF(L));abline(v=c(L,H))
      c(L, H, dF(c(L,H)), diff(pF(c(L,H))))
}
