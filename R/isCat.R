isCat <-
function (x) 
{
    if(!is.numeric(x)) return(TRUE)
    if(length(unique(x))<length(x)/5) {
      if(length(unique(x))>20) return(FALSE)
      else return(TRUE)
    }
    FALSE
}
