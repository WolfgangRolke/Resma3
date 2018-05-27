ihist <-
function(...) {
  require(shiny)
  data <- arrange.args(x=list(...), y=match.call())  
  df <- data[[1]][, data[[2]], drop=FALSE ]  
  attach(df)
  wrns <- c(data[[4]], ifelse(sum(data[[2]]) == 0, "None of the Variables provided is Numeric!!", ""))
  disps <- c("norm", "beta", "gamma", "exp", "f", "t", "chisq", "unif")
  names(disps) <- c("Normal", "Beta", "Gamma", "Exponential", "Snedecor's F", "Student's t", "Chi square", "Uniform") 

  out <- runApp(list(
        ui = fluidPage(
            tags$head(
              tags$style(HTML("
              table, th, td { text-align:right;}
              th, td {padding: 10px;}    
            "))
            ),
            titlePanel("Interactive Histogram"),
            sidebarLayout(
                sidebarPanel(
                  actionButton("doneButton", HTML("<h4><font color=\"blue\">Close App</font></h4>")),
                  selectInput("xvar", "Choose Numeric Variable", choices = colnames(df)),
                  uiOutput("choose_num_var"),
                  uiOutput("nbins"),
                  selectInput("dens","Add Theoretical Density?", choices=c("No","Yes")),
                  conditionalPanel( condition = "input.dens == 'Yes'",
                    radioButtons("f", "Density", choices = disps, selected = disps[1]),
                    textInput("par", "Parameters", value="1")       
                  ),
                  selectInput("nonpardens", "Add Non-parametric Density?",
                                 choices=c("No","Yes")),  
                  conditionalPanel( condition = "input.nonpardens == 'Yes'",
                      sliderInput("h", "Amount of Smoothing", min = 0.1, max=3, value = 1, step = 0.01)
                  ),               
                  selectInput("lbls", "Add / Change Labels?", choices=c("No","Yes")),  
                  conditionalPanel( condition = "input.lbls == 'Yes'",
                      textInput("label_x", "X Axis Label", value=""),
                      textInput("main_title", "Title", value="")
                  ),
                 selectInput("dosave", "Save Graph?", choices=c("No", "Yes")),
                 conditionalPanel( condition = "input.dosave == 'Yes'",
                   selectInput("whichformat","File Format",choices=c("png","pdf")),
                   textInput("folder","Folder",value="c:/"),
                   textInput("file","File Name (without extension)",value=""),
                   actionButton("saveButton", HTML("<h5>Click to save</h5>"))
                 )
                  
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel("Graph", 
                            uiOutput("textwrn"),
                            plotOutput("plt"),
                            uiOutput("normtext")),                
                        tabPanel("R Code",
                            wellPanel(uiOutput("text1")),
                            wellPanel(uiOutput("text2")))
                    )          
                )
           )   
        ), 
        server = function(input, output, session) {
            observe({
               if(input$doneButton > 0) 
                  stopApp(NULL)
            })
            
            output$textwrn <- renderText({                   
              lns <- ""
              if(wrns[1] != "") lns <- paste0(lns,"<h3>Warning: ",wrns[1],"</h3><p>")
              if(wrns[2] != "") {
                  lns <- paste0(lns,"<h3><font color=\"red\">",wrns[2],"</font></h3><p>")
              }    
              lns
            })
            

            output$nbins <- renderUI({
                nb <- round(2*length(df[, input$xvar])^(2/5))  
                nmax <- round(min(5*nb, length(df[, input$xvar])/5))              
                sliderInput("nbins", "Number of Bins", min = 5, max = nmax, value = nb, step = 5)
            })
            
            nb <- reactive({
                ifelse(input$nbins != round(2*length(df[, input$xvar])^(2/5)), paste0(", n = ", input$nbins), "")
            })
                
            output$normtext <- renderText({
                if(input$dens == "No") return("")
                if(input$f != "norm") return("")
                "<h4>If Parameters=1 uses mean and standard deviation of data</h4>"
            })
            
            fun <- reactive({
            
                lnsdens <- ""
                if(input$dens == "Yes") {
                    if(input$f == "norm" & input$par == "1") 
                        lnsdens <- paste0(", f=\"norm\", par=c(mean(", input$xvar,"), sd(", input$xvar,"))")
                    else {
                        param <- unlist(strsplit(input$par," "))
                        param <- as.numeric(param[param!=" "]) 
                        lnsdens <- paste0(", f = \"",input$f,"\", par = ")
                        if(length(param) == 1)        
                            lnsdens <- paste0(lnsdens, param) 
                        else    
                            lnsdens <- paste0(lnsdens, "c(",param[1],", ",param[2],")")
                    }    

                }
                xl <- ifelse(input$label_x != "", paste0(", label_x = \"",input$label_x,"\""),"")
                ml <- ifelse(input$main_title != "", paste0(", main_title = \"", input$main_title, "\""),"")
                ds <- ifelse(input$dens == "Yes", lnsdens, "")
                npd <- ifelse(input$nonpardens == "Yes", ", nonpar = TRUE", "")                
                h <- ifelse(input$h != 1, paste0(", h = ", input$h), "") 
                lns <- paste0("hplot(", input$xvar, nb(), xl, ml, ds, npd, h, ")")
              
                return(lns)
               
            })
            
            output$text1 <- renderText({
            
              lns0 <- paste0("<h3>Using Resma3</h3>")
              return(paste0("<h4>",fun(),"</h4>"))                    
            })
              
            output$text2 <- renderText({
                if(wrns[2] != "") return("")
                lns <- "<h4>Using ggplot2<p>"
                
                varname <-  input$xvar
                nbins <- input$nbins
                lns[2] <- paste("bw <- diff(range(",varname,"))/",nbins,"<br>",sep="")
                lns[3] <- paste("plt <- ggplot(data = data.frame(x = ",varname,"), aes(x)) 
                                        + geom_histogram(aes(y = ..density..), 
                                         color = \"black\", fill = \"white\", binwidth = bw)<br>",sep="")                        
                if(input$dens=="Yes") {
                    lns[length(lns)+1] <- paste("x1 <- seq(min(",varname,"), max(",varname,"), length = 100)<br>",sep="")
                    if(input$f == "norm" & input$par == "1") 
                        lns[length(lns)+1] <- paste("y1<-dnorm(x1,mean(",varname,"), sd(",varname,"))")
                    else {
                        param <- unlist(strsplit(input$par," "))
                        param <- as.numeric(param[param!=" "]) 
                        lns[length(lns)+1] <- paste("y1 <- d", input$f,"(x1, ",param[1], sep = "")
                        if(length(param)==2) 
                            lns[length(lns)] <- paste(lns[length(lns)],", ",param[2],sep="")
                        lns[length(lns)] <- paste(lns[length(lns)],")<br>",sep="")    
                    }    
                    lns[length(lns)+1] <- "plt <- plt + geom_line(data = data.frame(x = x1, y = y1), 
                                        aes(x, y), size = 1.5, colour = \"blue\")<br>" 
                }
                if(input$nonpardens == "Yes") {
                   h <- ifelse(input$h != 1, paste0(", h = ", input$h), "")
                   lns[length(lns)+1] <- paste0("plt <- plt + 
                      geom_line( stat=\"density\", size=1.2, color=\"red\"", h, ")<br>")  
                }   
                if(input$lbls == "Yes") {
                    if(input$label_x != "") 
                        lns[length(lns)+1] <- paste0("<br>plt <- plt + xlab(\"",input$label_x,"\")")
                    if(input$main_title != "") 
                        lns[length(lns)+1] <-  paste0("<br>plt <- plt + ggtitle(\"",input$main_title,"\") +
                                theme(plot.title = element_text(size=20, colour= \"blue\" ))")
                }
               
                lns[length(lns)+1] <- "<br>print(plt)</h4><hr>"
                lns    
            })  

            observeEvent(input$saveButton, {
                txt <- fun()
                txt <- paste0(substr(txt, start = 1, stop = nchar(txt)-1),", return.graph = TRUE)")
                f <- function(...) { eval(parse(text = txt))}

                file <- paste0(input$folder, "/", input$file, ".", input$whichformat)
                if(input$whichformat=="png")  png(file)
                else pdf(file)
                print(f())
                dev.off()
            })    
            
            output$plt <- renderPlot({             
              if(wrns[2] != "") return("")
              txt <- fun()
              txt <- paste0(substr(txt, start = 1, stop = nchar(txt)-1),", return.graph = TRUE)")
              f <- function(...) { eval(parse(text = txt))}
              return(f())
                
           })
        }
      ))
   detach(df)
   out
}
