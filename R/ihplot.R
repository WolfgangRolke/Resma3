ihplot <-
function(df) {
  require(shiny)
  xlbl <- deparse(substitute(df))
  if(is.data.frame(df)) {
      shinyApp(
        ui = fluidPage(
            sidebarLayout(
                sidebarPanel(
                  numericInput("n", "Number of Bins (0=Default)", value=0),
                  selectInput("var","Variable",choices=colnames(df)),
                  selectInput("dens","Add Density?",choices=c("No","Yes")),
                  conditionalPanel( condition = "input.dens == 'Yes'",
                        selectInput("densnorm","Normal Density?",choices=c("Yes","No")),
                        conditionalPanel( condition = "input.densnorm == 'No'",
                           textInput("f","Density",value="x"),
                           textInput("par","Parameters",value="1")
                        )   
                  ),
                  selectInput("nonpar","Add Nonparametric Density Estimate?",choices=c("No","Yes")),
                  textInput("ttl","Title of Graph",value=" ")                                                    
                ),
                mainPanel(
                    plotOutput("hist"),                
                    uiOutput("text1"),
                    uiOutput("text2")
                )
           )    
        ), 
        server = function(input, output) {
            output$text1 <- renderText({
                lns0 <- "<hr><h4>Using Resma3</h4>"
                lns <- paste("<h4>&gt; hplot(",input$var,sep="")
                if(input$n>0) lns <- paste(lns,", n=",input$n)
                 if(input$dens=="Yes") {
                    if(input$densnorm=="Yes") 
                        lns <- paste(lns,", f=\"norm\", par=c(mean(",input$var,"), sd(",input$var,"))")
                    else {
                        param <- unlist(strsplit(input$par," "))
                        param <- as.numeric(param[param!=" "]) 
                        lns <- paste(lns,", f=\" ",input$f," \", par=",sep="")
                        if(length(param)==1)        
                            lns <- paste(lns, param,sep="") 
                        else    
                            lns <- paste(lns,"c(",param[1],", ",param[2],")",sep="")
                    }    
                }
                if(input$ttl!=" ") 
                    lns <- paste(lns,", Title= \"",input$ttl," \"",sep="")
                if(input$nonpar=="Yes") 
                    lns <- paste(lns,", nonpar=TRUE",sep="")    
                lns <- paste(lns,")</h4><hr>",sep="")  
                c(lns0,lns)     
                    
            })  
            output$text2 <- renderText({
                varname<-input$var
                x <- df[,input$var]
                if(input$n==0) nbins<-round(2*length(x)^(2/5))
                else nbins<-input$n
                lns <- "<hr><h4>Using ggplot2<p>"
                lns[2] <- paste("&gt; bw <- diff(range(",varname,"))/",nbins,"<br>",sep="")
                lns[3] <- paste("&gt; plt <- ggplot(data = data.frame(x = ",varname,"), aes(x)) 
                                        + geom_histogram(aes(y = ..density..), 
                                         color = \"black\", fill = \"white\", binwidth = bw)<br>",sep="")                        
                if(input$dens=="Yes") {
                    lns[4] <- paste("&gt; x1 <- seq(min(",varname,"), max(",varname,"), length = 100))<br>",sep="")
                    if(input$densnorm=="Yes") 
                        lns[5] <- paste("&gt; y1<-dnorm(x1,mean(",varname,"), sd(",varname,"))")
                    else {
                        param <- unlist(strsplit(input$par," "))
                        param <- as.numeric(param[param!=" "]) 
                        lns[5] <- paste("&gt; y1 <- d", input$f,"(x1, ",param[1], sep = "")
                        if(length(param)==2) 
                            lns[5] <- paste(lns[5],", ",param[2],sep="")
                        lns[5] <- paste(lns[5],")<br>",sep="")    
                    }    
                    lns[6] <- paste("&gt; plt <- plt + geom_line(data = data.frame(x = x1, y = y1), 
                                        aes(x, y), size = 1.5, colour = \"blue\")<br>",sep="") 
                }
                if(input$ttl!=" ") 
                    lns[length(lns)+1] <- paste("&gt; plt <- plt + ggtitle(",input$ttl,")<br>",sep="")
                if(input$nonpar=="Yes") 
                    lns[length(lns)+1] <- "&gt; plt <- plt +  geom_density(size=1.2,color=\"red\")<br>"
                lns[length(lns)+1] <- "&gt; print(plt)</h4><hr>"
                lns    
            })  
            output$hist <- renderPlot({   
                x <- df[,input$var]
                if(input$n==0) nbins<-round(2*length(x)^(2/5))
                else nbins<-input$n
                if(input$dens=="Yes") {
                    if(input$densnorm=="Yes") {    
                        plt <- hplot(x, n = nbins, f="norm", par=c(mean(x), sd(x)), return.graph=TRUE)
                    }
                    else {
                        param <- unlist(strsplit(input$par," "))
                        param <- as.numeric(param[param!=" "])
                        plt <- hplot(x, n = nbins, f=input$f, par=param, return.graph=TRUE)
                    }
                }
                else  plt <- hplot(x, n = nbins, return.graph=TRUE)
                if(input$nonpar=="Yes") plt <- plt + geom_density(size=1.2,color="red")
                if(input$ttl!=" ") plt <- plt + ggtitle(input$ttl)
                plt + xlab(input$var)
                plt
            })
        }
      )
   } 
   else {
      shinyApp(
          ui = fluidPage(
              sidebarLayout(
                  sidebarPanel(
                      numericInput("n", "Number of Bins (0=Default)", value=0),
                      selectInput("dens","Add Density?",choices=c("No","Yes")),
                      conditionalPanel( condition = "input.dens == 'Yes'",
                          selectInput("densnorm","Normal Density?",choices=c("Yes","No")),
                          conditionalPanel( condition = "input.densnorm == 'No'",
                              textInput("f","Density",value="x"),
                              textInput("par","Parameters",value="1")
                        )   
                      ),
                      selectInput("nonpar","Add Nonparametric Density Estimate?",choices=c("No","Yes")),
                      textInput("ttl","Title",value=" ")                                                     
                  ),
                  mainPanel(
                      plotOutput("hist"),                
                      uiOutput("text1"),
                      uiOutput("text2")
                  )
              )    
          ), 
          server = function(input, output) {
            output$text1 <- renderText({
                lns0 <- "<hr><h4>Using Resma3</h4>"
                lns <- paste("<h4>&gt; hplot(",xlbl,sep="")
                if(input$n>0) lns <- paste(lns,", n=",input$n)
                 if(input$dens=="Yes") {
                    if(input$densnorm=="Yes") 
                        lns <- paste(lns,", f=\"norm\", par=c(mean(",xlbl,"), sd(",xlbl,"))")
                    else {
                        param <- unlist(strsplit(input$par," "))
                        param <- as.numeric(param[param!=" "]) 
                        lns <- paste(lns,", f=\" ",input$f," \", par=",sep="")
                        if(length(param)==1)        
                            lns <- paste(lns, param,sep="") 
                        else    
                            lns <- paste(lns,"c(",param[1],", ",param[2],")",sep="")

                    }    
                }
                if(input$ttl!=" ") 
                    lns <- paste(lns,", Title= \"",input$ttl," \"",sep="")
                if(input$nonpar=="Yes") 
                    lns <- paste(lns,", nonpar=TRUE",sep="")    
                lns <- paste(lns,")</h4><hr>",sep="")  
                c(lns0,lns)     
                    
            })  
            output$text2 <- renderText({
                varname<-xlbl
                x <- df
                if(input$n==0) nbins<-round(2*length(x)^(2/5))
                else nbins<-input$n
                lns <- "<hr><h4>Using ggplot2<p>"
                lns[2] <- paste("&gt; bw <- diff(range(",varname,"))/",nbins,"<br>",sep="")
                lns[3] <- paste("&gt; plt <- ggplot(data = data.frame(x = ",varname,"), aes(x)) 
                                        + geom_histogram(aes(y = ..density..), 
                                         color = \"black\", fill = \"white\", binwidth = bw)<br>",sep="")                        
                if(input$dens=="Yes") {
                    lns[4] <- paste("&gt; x1 <- seq(min(",varname,"), max(",varname,"), length = 100))<br>",sep="")
                    if(input$densnorm=="Yes") 
                        lns[5] <- paste("&gt; y1<-dnorm(x1,mean(",varname,"), sd(",varname,"))")
                    else {
                        param <- unlist(strsplit(input$par," "))
                        param <- as.numeric(param[param!=" "]) 
                        lns[5] <- paste("&gt; y1 <- d", input$f,"(x1, ",param[1], sep = "")
                        if(length(param)==2) 
                            lns[5] <- paste(lns[5],", ",param[2],sep="")
                        lns[5] <- paste(lns[5],")<br>",sep="")    
                    }    
                    lns[6] <- paste("&gt; plt <- plt + geom_line(data = data.frame(x = x1, y = y1), 
                                        aes(x, y), size = 1.5, colour = \"blue\")<br>",sep="") 
                }
                if(input$ttl!=" ") 
                    lns[length(lns)+1] <- paste("&gt; plt <- plt + ggtitle(",input$ttl,")<br>",sep="")
                if(input$nonpar=="Yes") 
                    lns[length(lns)+1] <- "&gt; plt <- plt +  geom_density(size=1.2,color=\"red\")<br>"
                lns[length(lns)+1] <- "&gt; print(plt)</h4><hr>"
                lns    
            })            
              output$hist <- renderPlot({ 
                x <- df              
                if(input$n==0) nbins<-round(2*length(x)^(2/5))
                else nbins<-input$n
                if(input$dens=="Yes") {
                    if(input$densnorm=="Yes") {
                        plt <- hplot(x, n = nbins, f="norm", par=c(mean(x), sd(x)), return.graph=TRUE)
                    }
                    else {
                        param <- unlist(strsplit(input$par," "))
                        param <- as.numeric(param[param!=" "])
                        plt <- hplot(x, n = nbins, f=input$f, par=param, return.graph=TRUE)
                    }
                }
                else  plt <- hplot(x, n = nbins, return.graph=TRUE)
                plt <- plt + xlab(xlbl)
                if(input$nonpar=="Yes") plt <- plt + geom_density(size=1.2,color="red")
                if(input$ttl!=" ") plt <- plt + ggtitle(input$ttl)
                plt
              })
        }
      )
   } 
}
