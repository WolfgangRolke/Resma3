tukey <-
function (y, x, z, which="first", with.interaction=TRUE, 
          ndigit=4, show.all=FALSE) 
{
    if (!is.vector(y)) return("First Argument has to be Numeric Vector") 
    if(missing(x)) return("Needs categorical variable!")
    varNames <- c(deparse(substitute(y)), deparse(substitute(x)))  
    means<-sort(tapply(y,x,mean))   
    xgroupnames<-names(means)
    newx <- factor(x,levels=xgroupnames,ordered=TRUE)
    if(missing(z)) {
        dta <- data.frame(y=y, x = newx)        
        fit <- aov(y~x,data=dta)
        tuk <- TukeyHSD(fit)$x
    }
    else {
        varNames <- c(varNames,deparse(substitute(z))) 
        means <- sort(tapply(y,z,mean))   
        zgroupnames <- names(means)
        newz <- factor(z,levels=zgroupnames,ordered=TRUE)
        dta <- data.frame(y=y, x = newx, z=newz)
        if(with.interaction) {
          if(max(table(x,z))==1) {
              txt3 <- "No repeated measurement!<br>Interaction term can not be included<br>"
              cat("No repeated measurement!\nInteraction term can not be included\n\n")
              with.interaction<-FALSE
          }      
        }          
        if(with.interaction) 
          fit <- aov(y~x*z,data=dta)
        else fit <- aov(y~x+z,data=dta) 
        tuk <- TukeyHSD(fit)   
        if(which=="first") tuk <- tuk[[1]]
        if(which=="second") {
              tuk <- tuk[[2]]
              groupnames <- zgroupnames
        }      
        if(which=="interaction" & with.interaction) 
              tuk <- tuk[[3]]
    }  
    pvals <- tuk[,4]
    if(!show.all) {
        pvals <- pvals[pvals<0.05]
        if(length(pvals)==0) 
            return("No Differences detected!")
    }    
    nm <- names(pvals)
    pvals <- as.numeric(pvals)
    pvals <- ifelse(pvals<0.001,0,pvals)
    out <- data.frame(Groups=nm,p.value=round(pvals,ndigit))
    if(!show.all) 
        cat("Groups that are statistically significantly different:\n")
    out
}
