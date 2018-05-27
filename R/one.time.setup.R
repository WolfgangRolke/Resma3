one.time.setup <-
function(echo = TRUE) {
  pcks <- c("Rcpp", "ggplot2", "shiny", "leaps", "ggExtra")
  counter <- 0
  for(p in pcks) {
    if(echo) cat("Checking package ",p," ...\n")
    if( !(p %in% rownames(installed.packages()))) {
       install.packages(p, repos=c(CRAN = "http://cran.rstudio.com/"))
      if(echo) cat("Succefully installed package ", p, "\n")  
        if(p == "rio") {
            vrs <- as.numeric(substring(unlist(R.Version())["version.string"], 13, 15))
            if(vrs>=3.0) {
                   library(rio)
               install_formats()
            }
        }
        counter <- counter + 1
        
    } 
    else {
      if(echo) cat(p, "is already installed", "\n")  
        counter <- counter + 1
    }
  }  
  if(counter == length(pcks))
    if(echo) cat("All necessary packages have been installed!\n")
  else 
    if(echo) cat("Some packages are not installed correctly.\n
           rerun one.time.setup and not where it stops!\n")
      
}
