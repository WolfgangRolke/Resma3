normal.check <-
function() {
    require(base64)
    category <- "Normal / Check"
    quizname <- " problem -"
    n <- 100
    type <- sample( 1:4, 1, prob = c(3,1,1,1))
    if(type == 1) x<- rnorm(100, 10, 3)
    if(type == 2) x<- rexp(100, 1)
    if(type == 3) x<- rt(100, 1)
    if(type == 4) x<- rbeta(100, 5, 1)
    if(type == 1) qtxt <- "{2:MC:~=Yes~No}"
    else qtxt <- "{2:MC:~Yes~=No}"
    plt <- nplot(x, return.graph = TRUE)
    plt64 <- png64(plt)
    qtxt <- paste0("<h5>Is this data from a normal distribution?<p>", 
                plt64, "<p>", qtxt, "</h5>")
    atxt <- "" 
    list(qtxt = qtxt, atxt = atxt, category = category, quizname = quizname)    
}
