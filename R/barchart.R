barchart <-
function (y, Percent, new.order, Polygon=FALSE, return.graph = FALSE) 
{
    yName <- deparse(substitute(y))
    ylbl <- ifelse(missing(Percent),"Count","Percentage")
    z <- y
    if(length(dim(y))==1) {
         if(!missing(Percent)) z<-z/sum(z)*100
         dta <- data.frame(y=as.numeric(z),x=names(z))
         plt <- ggplot(aes(x=x,y=y),data=dta)+
              geom_bar(stat="identity",fill="lightblue")+
              xlab(names(dimnames(y)))+ylab(ylbl) 
         if(Polygon) plt <- plt+ geom_freqpoly(aes(x,group=1),stat="identity",size=1.2,colour="red") 
              
         if(!missing(new.order)) plt <- plt +       
              scale_x_discrete(limits=names(z)[new.order])     
    }        
    else {    
        if(!missing(Percent)) {
            if(Percent=="Grand")  z<-z/sum(z)*100
            if(Percent=="Row")  for(i in 1:nrow(z)) z[i,]<-z[i,]/sum(z[i,])*100
            if(Percent=="Column")  for(i in 1:ncol(z)) z[, i]<-z[,i]/sum(z[,i])*100         
        }
        dta<-data.frame(y=as.numeric(z),x1=rep(rownames(z),ncol(z)),x2=rep(colnames(z),each=nrow(z)))    
        plt <- ggplot(aes(x=x1,y=y,fill=factor(x2)),data=dta)+ 
              geom_bar(position="dodge",stat="identity")+
              xlab(names(dimnames(y))[1])+ylab(ylbl)+theme(legend.title=element_blank())         
    }       

    if (return.graph) return(plt)         
    else print(plt)

}
