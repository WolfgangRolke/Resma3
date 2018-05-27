splot <-
function (y, x, z, w, use.facets = FALSE, add.line = 0, plot.points = TRUE, jitter=FALSE, errorbars=FALSE, 
          label_x, label_y, label_z, main_title, add.text, add.text_x,add.text_y,
          plotting.symbols=NA, plotting.size=1, plotting.colors=NA, ref_x, ref_y, 
          log_x = FALSE, log_y=FALSE,
          no.legends = FALSE, return.graph = FALSE) 
{    
    if (!is.vector(y)) 
        return(cat("First Argument has to be Numeric Vector\n"))
    if (!is.vector(x)) 
        return(cat("Second Argument has to be Numeric Vector\n\n"))
    noCatVar <- ifelse(missing(z), TRUE, FALSE)
    noCatVar2 <- ifelse(missing(w), TRUE, FALSE)
    varNames <- c(deparse(substitute(y)), deparse(substitute(x)), 
        ifelse(noCatVar, "", deparse(substitute(z))), 
        ifelse(noCatVar2, "", deparse(substitute(w))))
    
    if (noCatVar) z <- rep("0", length(x))
    if (is.numeric(z)) z <- as.factor(z)
    if (noCatVar2) w <- rep("0", length(x))
    if (is.numeric(w)) w <- as.factor(w)
    dta <- data.frame(x = x, y = y, z = z, w=w)
    if (noCatVar | use.facets) {
          plt <- ggplot(aes(x, y), data = dta) 
          if(plot.points) {
              if(is.na(plotting.symbols) & is.na(plotting.colors))
                  plt <- plt + geom_point(size=plotting.size)
              if(!is.na(plotting.symbols) & is.na(plotting.colors))
                  plt <- plt + geom_point(size=plotting.size,shape=plotting.symbols)
              if(is.na(plotting.symbols) & !is.na(plotting.colors))
                  plt <- plt + geom_point(size=plotting.size,colour=plotting.colors)
              if(!is.na(plotting.symbols) & !is.na(plotting.colors))
                  plt <- plt + geom_point(size=plotting.size,shape=plotting.symbols,colour=plotting.colors)          
          }
    }      
    else {
      if(is.na(plotting.symbols[1]) & is.na(plotting.colors[1])) 
          plt <- ggplot(aes(x, y, colour = z), data = dta) + 
                 geom_point(size=plotting.size) 
      if(!is.na(plotting.symbols[1]) & is.na(plotting.colors[1])) 
          plt <- ggplot(aes(x, y, colour = z, shape=z), data = dta) + 
                 geom_point(size=plotting.size) +
                 scale_shape_manual(values=plotting.symbols) 
      if(is.na(plotting.symbols[1]) & !is.na(plotting.colors[1])) 
          plt <- ggplot(aes(x, y, colour = z), data = dta) + 
                 geom_point(size=plotting.size) +
                 scale_colour_manual(values=plotting.colors)
      if(!is.na(plotting.symbols[1]) & !is.na(plotting.colors[1])) 
          plt <- ggplot(aes(x, y, colour = z, shape=z), data = dta) + 
                 geom_point(size=plotting.size) +
                 scale_shape_manual(values=plotting.symbols) + 
                 scale_colour_manual(values=plotting.colors)           
      plt <- plt + guides(color = guide_legend(title = ifelse(missing(label_z),varNames[3],label_z)))
    }
    plt <- plt + xlab(ifelse(missing(label_x),varNames[2],label_x)) + 
                 ylab(ifelse(missing(label_y),varNames[1],label_y))
    if(use.facets) plt <- plt + facet_wrap(~z)
    if(!noCatVar2) plt <- plt + facet_wrap(~w)
  
    if(!missing(main_title)) plt <- plt + ggtitle(main_title) +
          theme(plot.title = element_text(size=20,colour="blue"))
    if(jitter) plt <- plt + geom_jitter()     
    if (add.line > 0) {
        if (add.line < 3)
            plt <- plt + geom_smooth(method = c("lm", "loess")[add.line], se = errorbars)
        if(add.line == 3) plt<-plt+geom_line()            
    }
    if(!missing(ref_y))
      plt <- plt + geom_hline(yintercept=ref_y,linetype=1)
    if(!missing(ref_x))
      plt <- plt + geom_vline(xintercept=ref_x,linetype=1)
    if(no.legends==TRUE)
      plt <- plt + theme(legend.position="none")
    if(!missing(add.text))
      plt <- plt + annotate("text",x = add.text_x, y=add.text_y, label=add.text)
    if(log_x)
        plt <- plt + scale_x_log10() 
    if(log_y)
       plt <- plt + scale_y_log10() 
    if(return.graph) return(plt)
    else print(plt)
 
}
