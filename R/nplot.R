nplot <-
function (y, x, maintitle, main_title, return.graph = FALSE) 
{
    if(missing(x)) x<-rep(1,length(y))
    if (is.vector(x)) fit<-lm(y~x)
    else fit<-lm(x[,2]~x[,1])   
    plt<-ggQQ(fit)
    if(!missing(main_title)) plt <- plt + ggtitle(main_title) +
        theme(plot.title = element_text(size=20,colour="blue"))
    if(return.graph) return(plt)
    else print(plt)

}
