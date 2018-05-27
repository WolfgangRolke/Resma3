kruskalwallis <-
function(y,x,return.result=FALSE) {
    pval <- as.numeric(unlist(kruskal.test(y~factor(x)))[3])
    names(pval) <- "p value"
    if(return.result) return(pval)
    if(pval<0.001) pval<-"0.000"
    txt1<-paste("p value of test of equal means: p =",pval)
    cat(txt1,"\n")

}
