iplot <-
function (y, x, z,return.graph=FALSE) 
{
  varNames <- c(deparse(substitute(y)), deparse(substitute(x)),deparse(substitute(z)))
  if(is.numeric(x)) x<-factor(x)
  if(is.numeric(z)) z<-factor(z)
  dta <- data.frame(x=x,y=y,z=z)
  plt <- ggplot(data = dta,
       aes(x , y, colour = z, group=z)) +
      stat_summary(fun.y=mean, geom="point")+
      stat_summary(fun.y=mean, geom="line")+ xlab(varNames[2]) + ylab(varNames[1]) + 
            guides(color = guide_legend(title = varNames[3]))
  if(return.graph) return(plt)
  else print(plt)

}
