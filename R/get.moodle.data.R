get.moodle.data <-
function(sep="", header=TRUE, is.table=FALSE) {
  options(warn=-1)
  txt <- "Data has been attached"
  if(is.table) {
    df <- read.table("clipboard", header=header)
  }
  else {
    x <- scan("clipboard", what="character", sep=sep, quiet=TRUE)
    if(any(is.na(as.numeric(x))) & any(!is.na(as.numeric(x)))) {
      df <- read.table("clipboard", header=header)
    }
    else {
      if(all(!is.na(as.numeric(x)))) {
        txt <- ""
        df <- as.numeric(x)    
      }  
      else {
        z <- table(x)
        z <- z[z==1]
        if(length(z)>1) 
          df <- read.table("clipboard", header=header)
        else  {
          txt <- ""
          df <- x
        }
          
      }
    }
  }
  repeat {
    if("x" %in% search()) detach(x) 
    else break
  }
  cat("Data begins with:\n")
  print(head(df))
  cat("\n")
  assign("x", df, pos = .GlobalEnv)
  cat("Data has been saved as x\n\n")
  if(txt!="") {
    attach(df, pos=-1, name="x")
    cat(txt);cat("\n")
  }
  options(warn=0)
   
}
