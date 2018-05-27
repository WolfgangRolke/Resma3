dlr <-
function (y, x, z, additive=FALSE, show.tests=FALSE, 
          return.model=FALSE, return.result=FALSE, ndigit=3) 
{
    if (!is.vector(y)) return("First Argument has to be Numeric Vector") 
    if(!is.vector(x)) return("Second Argument has to be Numeric Vector") 
    varNames <- c(deparse(substitute(y)), deparse(substitute(x)), deparse(substitute(z)))
    zvals<-unique(z)
    if(length(zvals)>2) {
          cat("Routine can only handle one binary variable!\n")
          return(NULL)
    }      
    zCode<- ifelse(z==zvals[1],0,1)
    dta<-data.frame(x=x,y=y,z=zCode)
    if(additive) {
           fit<-lm(y~x+z,data=dta)
           m <- 2
     }      
     else  {  
           fit<-lm(y~x*z,data=dta)
           m <- 3
    }
    if(return.model) return(fit)
    txt1 <- "The least squares regression equation is:"
    cf <- round(coef(fit),ndigit)    
    txt1[2]<-paste(varNames[1]," = ",cf[1])
    sgn <- ifelse(cf[2]>0,"+","-")   
    txt1[2]<-paste(txt1[2],sgn,abs(cf[2]),varNames[2])
    sgn <- ifelse(cf[3]>0,"+","-")   
    txt1[2]<-paste(txt1[2],sgn,abs(cf[3]),varNames[3])
    if(!additive) {
        sgn <- ifelse(cf[4]>0,"+","-")   
        txt1[2]<-paste(txt1[2]," ",sgn," ",abs(cf[4])," ",varNames[2],"*",varNames[3],sep="")
    }
    names(cf)[1:3] <- c("Intercept",varNames[2],varNames[3]) 
    if(!additive) names(cf)[4] <- paste(varNames[2],"*",varNames[3],sep="")
    if(return.result) cf
    
    cat(txt1[1],"\n",txt1[2],"\n")
    dta1<-data.frame(x=fitted(fit),y=residuals(fit),z=z)
    plt1<-ggplot(aes(x, y,colour=z), data = dta1) + 
                geom_point() + 
                xlab("Fitted Values") + ylab("Residuals")+
                geom_hline(yintercept=0,colour="blue")+
                geom_smooth(method="lm",se=F)+
                guides(color = guide_legend(title = varNames[3]))
    plt2<-ggQQ(fit)
    multiple.graphs(plt1, plt2)
    pvals <- round(summary(fit)[[4]][,4],4)
    pvals <-ifelse(pvals<0.001,"0.000",pvals)
    outtxt <- data.frame(rep("",m+1),rep(0,m+1))
    colnames(outtxt) <- c("Variable","p value")
    outtxt[1,1] <- "Constant"
    outtxt[1,2] <- pvals[1]   
    for(i in 1:m) {
         outtxt[i+1,1] <- names(cf)[i+1]
         outtxt[i+1,2] <- pvals[i+1]
    }
    if(show.tests) print(outtxt, row.names=F) 
    txtR2<-paste("R^2 =",round(100*unlist(summary(fit))$r.squared,1)) 
    cat(txtR2,"\n") 

}
