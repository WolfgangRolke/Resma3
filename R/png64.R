png64 <-
function(plt) {
  pngfile <- tempfile()
  png(pngfile, width = 400, height = 400)
  print(plt)
  dev.off()
  pltout <- img(pngfile, Rd = TRUE, alt = "a")
  m <- nchar(pltout)
  pltout <- substring(pltout, 6, m-1)
  pltout
}
