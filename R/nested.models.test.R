nested.models.test <-
function (bigmodel, smallmodel, return.result=FALSE) 
{
    pval <- round(anova(bigmodel, smallmodel)[2, 6], 4) 
    if(return.result) return(pval)
    if(pval<0.001) pval <- "0.000"
    cat("H0: both models are equally good.\n")
    cat("p value=", pval,"\n")

}
