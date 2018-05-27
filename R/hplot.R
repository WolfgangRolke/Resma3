hplot <-
function (x, f, par, n, nonpar=FALSE, h = 1, label_x, 
            main_title, return.graph = FALSE) 
{
    varName <- ifelse(missing(label_x), deparse(substitute(x)), label_x)
    if (missing(n)) 
        n <- 2 * sqrt(length(x))
    bw <- diff(range(x))/n
    plt <- ggplot(data = data.frame(x = x), aes(x)) + geom_histogram(aes(y = ..density..), 
        color = "black", fill = "white", binwidth = bw) + xlab(varName) + 
        ylab("")
    if (!missing(f)) {
        df <- get(paste("d", f, sep = ""))
        x1 <- seq(min(x), max(x), length = 100)
        if (length(par) == 1) 
            y1 <- df(x1, par)
        else y1 <- df(x1, par[1], par[2])
        dta <- data.frame(x = x1, y = y1)
        if(is.numeric(y1))
            plt <- plt + geom_line(data = dta, aes(x, y), size = 1.25, 
              colour = "blue")            
    }
    if(!missing(main_title)) plt <- plt + ggtitle(main_title) +
      theme(plot.title = element_text(size=20,colour="blue"))
    if(nonpar) plt <- plt + 
        geom_line( stat="density", size=1.2, color="red", adjust = h)
     if (return.graph) return(plt)
     else print(plt)
    
}
