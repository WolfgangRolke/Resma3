getx <-
function(sep="") {
  options(warn=-1)
  x <- scan("clipboard", what="character", sep=sep)
  if(all(!is.na(as.numeric(x))))
    x <- as.numeric(x)
  options(warn=0)
  x  
}
