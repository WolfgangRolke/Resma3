bplot <-
function (y, x, orientation = "Vertical", new_order,  
          do.violin = FALSE, label_x, label_y, 
          main_title, return.graph = FALSE) 
{
    if (!is.vector(y)) return("First Argument has to be Numeric Vector") 
    label_y <- ifelse(missing(label_y), deparse(substitute(y)), label_y)
    if(missing(x)) {
        dta <- data.frame(y = y, x = rep(" ", length(y)))
        varNames <- c(" ", label_y)
    }
    else {
        label_x <- ifelse(missing(label_x), deparse(substitute(x)), label_x)
        varNames <- c(label_y, label_x)
        y <- y[order(x)]
        x <- x[order(x)]
        if(missing(new_order)) new_order <- 1:length(unique(x))
        if(new_order[1]=="Size") 
          new_order <- order(tapply(y, x, mean))
        x <- factor(x,levels=unique(x)[new_order], ordered=TRUE)
        dta <- data.frame(x = x, y = y)
    }
    plt <- ggplot(aes(x, y), data = dta)  + 
            xlab(varNames[2]) + ylab(varNames[1])  
    if(!missing(main_title))
      plt <- plt + ggtitle(main_title) +
      theme(plot.title = element_text(size=20,colour="blue"))
    if(do.violin)
            plt <- plt + geom_violin()
    else
            plt <- plt + geom_boxplot()
    if(orientation == "Horizontal")
            plt <- plt + coord_flip()
    if(return.graph) return(plt)         
    else print(plt)
    
}
