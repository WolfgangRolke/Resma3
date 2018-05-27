flplot <-
function (y, x, z, additive=FALSE, logx=FALSE,logy=FALSE, polydeg=1, 
    jitter=FALSE, return.graph = FALSE) 
{
    if (!is.vector(y)) 
        return(cat("First Argument has to be Numeric Vector\n"))
    if (!is.vector(x))
          return(cat("Second Argument has to be Numeric Vector\n"))    
    varNames <- c(deparse(substitute(y)), deparse(substitute(x)))
    if(!missing(z)) {
        varNames[3]<-deparse(substitute(z))
        if(!additive) {
            dta <- data.frame(x = x, y = y, z=z)
        plt <- ggplot(aes(x,y,colour=z),data=dta)+geom_point()+geom_smooth(method="lm",se=F)+
              xlab(varNames[2])+ylab(varNames[1])+ 
              guides(color = guide_legend(title = varNames[3]))            
        }
        else {
            txt0 <- paste("<p>> <em>flplot(",varNames[1],",",varNames[2],",",varNames[3],",additive=FALSE)</em><p>")
            zvals<-unique(z)
            m<-length(zvals)
            zCode<-rep(0,length(z))
            for(i in 2:m) zCode[z==zvals[i]]<-i-1
            dta <- data.frame(x = x, y = y, z=z)
            cf<-coef(lm(y~x+zCode))
            mycols<-c("red","blue","green","black","orange")
            plt <- ggplot(aes(x,y,colour=z),data=dta)+geom_point()+
                xlab(varNames[2])+ylab(varNames[1])+ 
                scale_colour_manual(values = mycols[1:m])+
                guides(color = guide_legend(title = varNames[3]))+
                geom_point(aes(x,y),data=data.frame(x=x[zCode==0],y=y[zCode==0]),colour=mycols[1])+
                geom_abline(intercept=cf[1],slope=cf[2],colour=mycols[1])+
                geom_point(aes(x,y),data=data.frame(x=x[zCode==1],y=y[zCode==1]),colour=mycols[2])+
                geom_abline(intercept=cf[1]+cf[3],slope=cf[2],colour=mycols[2])
           if(m>2) 
                  for(i in 3:m)
                      plt <- plt+
                geom_point(aes(x,y),data=data.frame(x=x[zCode==i-1],y=y[zCode==i-1]),colour=mycols[i])+
                geom_abline(intercept=cf[1]+cf[i+1],slope=cf[2],colour=mycols[i])
        }
    }    
    else {
    dta <- data.frame(x = x, y = y)

    plt <- ggplot(aes(x, y), data = dta) + geom_point() + 
                xlab(varNames[2]) + ylab(varNames[1])
    if(jitter) plt <- plt+geom_jitter()  
    if(!logx & !logy & polydeg==1)  
          plt<-plt+geom_smooth(method="lm",se=F)

    if(logx | logy) {
         if(polydeg>1) return("Do not mix polynomials and logarithms!")
          xx<-seq(min(x),max(x),length=250)

          if(logx & !logy) {
              lx<-log(x)
              fit<-lm(y~lx)
              fity <- predict(fit,newdata=list(lx=log(xx)))
          }
          if(!logx & logy) {
              lx<-x
              fit<-lm(log(y)~lx)
              fity <- exp(predict(fit,newdata=list(lx=xx)))
          }
          if(logx & logy) {
              lx<-log(x)
              fit<-lm(log(y)~lx)
              fity<-exp(predict(fit,newdata=list(lx=log(xx))))
          }
          dta1<-data.frame(x=xx,y=fity)
          plt<-plt+geom_line(aes(x,y),data=dta1,size=1.2,colour="blue")

    }   
    if(polydeg>1) {
        ny<-varNames[2]
        varNames<-c(varNames[1],paste(varNames[1],"^",2:polydeg,sep=""),ny)
    
        xmat<-matrix(0,length(y),polydeg)
        colnames(xmat) <- varNames[1:polydeg]
        for(i in 1:polydeg) xmat[,i]<-x^i
        fit<-lm(y~xmat)    
        xx<-seq(min(x),max(x),length=250)
        newx<-matrix(0,250,polydeg)
        for(i in 1:polydeg) newx[,i]<-xx^i
        colnames(newx)<-colnames(x)
        fity <- predict(fit,newdata=list(xmat=newx))
        dta1<-data.frame(x=xx,y=fity)
        plt<-plt+geom_line(aes(x,y),data=dta1,size=1.2,colour="blue")
    }
    }         
    if(return.graph) return(plt)
    else print(plt)    
     
}
