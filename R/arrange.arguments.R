arrange.arguments <-
function(x, y) {
# data <- arrange.args(x=list(...), y=match.call())
# takes any number of inpuut and arranges them as a data frame  
        nvar <- length(x)
        nms <- paste(y)[-1]
        if(nvar == 1) data_name <- nms
        else data_name <- ""
        if(!is.null(dim(x[[1]]))) out <- x[[1]]
        else {
          out <- data.frame(x[[1]]) 
          colnames(out) <- nms[1]
        }   
        warn <- ""
        if(length(nms) > 1) { 
          for(i in 2:nvar) {
           if(!is.null(dim(x[[i]]))) out1 <- x[[i]]
           else {
              out1 <- data.frame(x[[i]])
              colnames(out1) <- nms[i]
           }  
           if(nrow(out)==nrow(out1)) {
              out <- data.frame(out,out1)                    
                
           } else warn <- "<h4>Some arguments have different lengths!! <br>
                      Only those matching first argument are used</h4><p>"
          } 
        }
        is_num <- rep(FALSE,dim(out)[2])
        for(i in 1:dim(out)[2]) {
            if(is.numeric(out[ ,i])) {
              is_num[i] <- TRUE
            } else {
              if(!is.factor(out[, i])) {
                out[ ,i] <- type.convert(out[, i], as.is = TRUE)
                if(is.numeric(out[, i]))
                    is_num[i] <- TRUE
              }  
            }
       }    
       list(data=out, is_num = is_num, data_name = data_name, warn = warn)
  }
