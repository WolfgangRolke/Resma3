hyptest.helper <- function(parameter, alternative, value) {
  if(alternative %in% c("two.sided", "not.equal", "!=", "<>")) {
    alternative <- "two.sided"
    sgn<-" <> "
  }
  if(alternative %in% c("less", "<")) {
      alternative <- "less"
      sgn <- " < "
  }    
  if(alternative %in% c("greater", ">")) {
    alternative <- "greater"
    sgn <- " > "
  } 
  txt <- paste0("p value of test H0: ", parameter, "=", value, 
        " vs. Ha: ", parameter, sgn, value, ": ")
  list(alternative=alternative, txt=txt)
}
