pearson.cor <-
function (y, x, rho.null, conf.level=95, ndigit=1, return.result = FALSE, 
               return.graph=FALSE, no.graph = FALSE) 
{
    yName <- deparse(substitute(y))
    xName <- deparse(substitute(x))
    hyp.info <- hyptest.helper("rho", "two.sided", ifelse(missing(rho.null), 0, rho.null))  
    
    results <- cor.test(x, y, conf.level=conf.level/100)
    r <- round(results$estimate,3)
    est <- paste("Pearson's correlation coeffcient is ",r)
    ci <- round(results$conf.int, 3)
    names(ci) <- c( "Low", "High" )
    pval <- round(results$p.value, 4)
    names(pval) <- "p value"
    if( return.result ) return( c(est, pval, ci) )    
    if( pval<0.001 ) pval <- "0.000" 
    plt <- mplot( y, x, return.graph = TRUE, varNames = c(yName, xName) )
    if(return.graph) return(plt)
    if(!no.graph) print(plt)
     
    if( missing(rho.null) ) {
         citxt <- paste0("A ", conf.level,"% confidence interval for the 
population correlation coefficient is ( ", ci[1],", ",ci[2]," )")
         cat( citxt, "\n") 
    }     
    else {
        pvaltxt <- paste(hyp.info$txt , pval)       
        cat( pvaltxt, "\n")    
    }


}
