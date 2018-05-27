getfun <-
function(txt, add.return.result  = TRUE ) {
    if(add.return.result)  {
        for(i in nchar(txt):1) {
            if(substring(txt, first = i, last = i) == ")") {
                 txt <- paste0(substring(txt, first = 1, last = i-1),", return.result = TRUE)")
                 break
            }
        }                  
    }                        
    f <- function(...) { eval(parse(text = txt))}
    f
}
