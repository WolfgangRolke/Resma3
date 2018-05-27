sigdigits <-
function(x) length(gregexpr("[[:digit:]]", as.character(x))[[1]])
