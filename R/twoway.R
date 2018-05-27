twoway <-
function (y, x, z, with.interaction=TRUE, return.result=FALSE, ndigit=1) 
{
    varNames <- c(deparse(substitute(y)), deparse(substitute(x)), deparse(substitute(z)))
    if (is.numeric(x)) x <- as.factor(x)
    if (is.numeric(z)) z <- as.factor(z)
    dta <- data.frame(x = x, y = y, z=z)
    if (!is.vector(y)) return("First Argument has to be Numeric Vector") 
    txt3 <- ""
    if(with.interaction) {
        if(max(table(x,z))==1) {
              txt3 <- "No repeated measurement! Interaction term can not be included"
              cat(txt3,"\n")
              with.interaction <- FALSE
        }      
    }          
    if(with.interaction) fit <- aov(y~x*z, data=dta)
    else fit <- aov(y~x+z, data=dta)
    print(summary(fit))
    pvals <- round(summary(fit)[[1]][,5],4)
    pvals <- ifelse(pvals<0.001,0.000,pvals)
    names(pvals)[1:2] <- paste(varNames[2:3]," p =")
    if(with.interaction)  
        names(pvals)[3] <- paste("Interaction p =")
    pvals <- cbind(pvals[1:(length(pvals)-1)])
    if(return.result) return(pvals)
    print(pvals)  
    pvals <- ifelse(pvals<0.001,"0.000",pvals)  
    dta1 <- data.frame(x=fitted(fit),y=residuals(fit))   
    plt1 <- ggplot(aes(x, y), data = dta1) + 
          geom_point() + 
          xlab("Fitted Values") + ylab("Residuals")+
          geom_hline(yintercept=0,colour="blue")
    plt2 <- ggQQ(fit)
    multiple.graphs(plt1,plt2)
      
}
