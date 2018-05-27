mplot <-
function (y, x, z, add.line = 0, varNames, return.graph = FALSE) 
{
    noCatVar <- ifelse(missing(z), TRUE, FALSE)
    if(missing(varNames))
        varNames <- c(deparse(substitute(x)), deparse(substitute(y)), 
        ifelse(noCatVar, "", deparse(substitute(z))))
    if (noCatVar) z <- rep("0", length(x))
    if (is.numeric(z)) z <- as.factor(z)
    dta <- data.frame(x = x, y = y, z = z)
    
    if (noCatVar) 
        plt <- ggplot(aes(x, y), data = dta) + geom_point() + 
                xlab(varNames[1]) + ylab(varNames[2])
    else plt <- ggplot(aes(x, y, colour = z), data = dta) + 
         geom_point() + xlab(varNames[1]) + ylab(varNames[2]) + 
         guides(color = guide_legend(title = varNames[3]))
    
    if (add.line > 0) 
    plt <- plt + geom_smooth(method = c("lm", "loess")[add.line], 
                se = F)

    plt1<-ggExtra::ggMarginal(plt,data=dta,type="boxplot")
    if (return.graph) return(plt1)
    else print(plt1)

}
