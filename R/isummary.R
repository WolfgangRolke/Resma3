isummary <-
function(...) {
  require(shiny)
  data <- arrange.args(x=list(...), y=match.call())
  df <- data[[1]]
  attach(df)
  wrns <- c(data[[4]], ifelse(sum(data[[2]]) == 0, "None of the Variables provided is Numeric!!", ""))
  names(wrns) <- c("bad lengths","novar")
  if(sum(data[[2]])>0) numVars <- colnames(data[[1]])[data[[2]]]

  out <- runApp(list(
        ui = fluidPage(
            tags$head(
              tags$style(HTML("
              table, th, td { text-align:right;}
              th, td {padding: 10px;}    
            "))
            ),
            titlePanel("Interactive Data Summaries App"),
            sidebarLayout(
                sidebarPanel(
                  actionButton("doneButton", HTML("<h4><font color=\"blue\">Close App</font></h4>")),
                  selectInput("whichgraph", "Which Graph?", choices=c("Boxplot","do.violinplot")),
                  uiOutput("choose_num_var"),
                  uiOutput("choose_cat_var"),
                  conditionalPanel( condition = "input.whichgraph == 'Histogram'",
                        numericInput("n", "Number of Bins (0=Default)", value=0),
                        selectInput("dens","Add Density?",choices=c("No","Yes")),
                        conditionalPanel( condition = "input.dens == 'Yes'",
                            selectInput("densnorm","Normal Density?",choices=c("Yes","No")),
                            conditionalPanel( condition = "input.densnorm == 'No'",
                                textInput("f","Density",value="x"),
                                textInput("par","Parameters",value="1")
                            )   
                        )    
                  ),
                  conditionalPanel( condition = "input.whichgraph != 'Histogram'",
                      selectInput("orientation","Orientation",choices=c("Vertical","Horizontal"))
                  ),
                  uiOutput("ord"),
                  selectInput("lbls","Add / Change Labels?",choices=c("No","Yes")),  
                  conditionalPanel( condition = "input.lbls == 'Yes'",
                    textInput("label_x","X Axis Label",value=""),
                    uiOutput("ylbl"),
                    textInput("main_title","Title",value="")
                  ),
                  numericInput("ndigit","Number of significant digits",value=1,step=1), 
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
                            plotOutput("plt")),                
                        tabPanel("Numerical Summaries",
                            uiOutput("summarytable1"),
                            uiOutput("summarytable2"),
                            uiOutput("summarytable3")),
                        tabPanel("R Code",
                            uiOutput("text1"),
                            uiOutput("text3"),
                            uiOutput("text2"))
                    )          
                )
           ), width=4    
        ), 
        server = function(input, output) {
            observe({
               if(input$doneButton > 0) 
                  stopApp(NULL)
            })
            
            output$ylbl <- renderUI({
                if(input$lbls == "No") return("")
                if(yvar_name() != "") return(textInput("label_y","Y Axis Label",value=""))
                ""
            })
            
            output$choose_num_var <- renderUI({
                if(length(numVars) == 1) return("")
                selectInput("varx","Choose Numeric Variable",choices=numVars)
            })
            
            output$choose_cat_var <- renderUI({          
                if(dim(df)[2] <= 2) return("")
                lst <- c("", colnames(df)[colnames(df)!=input$varx])
                selectInput("vary","Choose Categorical Variable (Optional)", choices=lst)
            })
                        
            output$ord <- renderUI({
                if(yvar_name() != "") return(textInput("new_order",
                          HTML("Use Ordering \n(use position numbers 1 2 ..)"),value=""))
                ""
            })
            
            xvar <- reactive({
                 if(length(numVars) == 1) return(c(df[, numVars]))
                 c(df[ ,input$varx])
            })

            xvar_name <- reactive({
                 if(length(numVars) == 1) return(numVars)
                 input$varx
            })

            yvar <- reactive({
                 if(ncol(df) == 1) return(NULL)
                 if(ncol(df) == 2) {
                      if(colnames(df)[1] == xvar_name()) other <- colnames(df)[2]
                        else other <- colnames(df)[1]
                        return(c(df[, other]))
                 }
                 c(df[ ,input$vary])
            })

            yvar_name <- reactive({
                 if(ncol(df) == 1) return("")
                 if(ncol(df) == 2) {
                      if(colnames(df)[1] == xvar_name()) other <- colnames(df)[2]
                        else other <- colnames(df)[1]
                        if(!is.numeric(df[, other])) return(other)
                        else return("")
                 }
                 input$vary
            })

            new_order <- reactive({
                  if(yvar_name() != "")
                    if(input$new_order !="") {
                      a <- unlist(strsplit(input$new_order," "))
                      a <- a[a!=""]
                      new_order <- as.numeric(a)  
                  }    
                  else new_order <- 1:length(unique(yvar()))
                  new_order
            })
            
            output$summarytable1 <- renderText ({
                  if(wrns[2] != "") return("")
                  if(yvar_name() == "") out <- stat.table(xvar(), ndigit = input$ndigit)
                  else out <- stat.table(xvar(),yvar(), new_order = new_order(), ndigit = input$ndigit)                
                  htbl(out, ifelse(yvar_name() == "", xvar_name(), yvar_name()))
            })
            
            output$summarytable2 <- renderText ({
                  if(wrns[2] != "") return("")
                  if(yvar_name() == "") out <- stat.table(xvar(), Mean=FALSE, ndigit = input$ndigit)
                  else out <- stat.table(xvar(),yvar(), Mean=FALSE, new_order = new_order(), ndigit = input$ndigit)
                  htbl(out, ifelse(yvar_name() == "", xvar_name(), yvar_name()))
             })
            
             output$summarytable3 <- renderText ({           
                  if(wrns[2] != "") return("")
                  if(yvar_name() == "") out <- fivenumber(xvar(), ndigit = input$ndigit, return.result = TRUE)
                  else out <- fivenumber(xvar(), yvar(), new_order = new_order(),  ndigit = input$ndigit, return.result = TRUE)
                  out <- htbl(out, ifelse(yvar_name() == "", xvar_name(), yvar_name()))
                  nmiss <- sum(is.na(df[, xvar_name()]))
                  lns <- ifelse(nmiss > 0, paste0("<p><h4>Warning: ",nmiss," missing values were removed!</h4>"), "")    
                  c(out,lns)
            })

            output$textwrn <- renderText({                   
              lns <- ""
              if(wrns[1] != "") lns <- paste0(lns,wrns[1],"<p>")
              if(wrns[2] != "") {
                  lns <- paste0(lns,"<h3><font color=\"red\">",wrns[2],"</font></h3><p>")
              }    
              lns
            })
            
            fun <- reactive({
                  yv <- ifelse(yvar_name() != "", paste0(", x = ",yvar_name()), "")
                  vl <- ifelse(input$whichgraph == "do.violinplot", ", do.violin = TRUE", "")
                  ho <- ifelse(input$orientation =="Horizontal", ", orientation = \"Horizontal\"", "") 
                  xl <- ifelse(input$label_x != "", paste0(", label_y = \"",input$label_x,"\""),"")
                  yl <- ifelse(input$label_y != "", paste0(", label_x = \"",input$label_y,"\""),"")
                  ml <- ifelse(input$main_title != "", paste0(", main_title = \"", input$main_title, "\""),"")
                  nw <- ""
                  if(yvar_name() != "")
                     if(input$new_order[1] != "") 
                        nw <- paste0(", new_order = c(", new_order()[1], paste0(", ",new_order()[-1],collapse=""),")")
                   
                  lns <- paste0("bplot(y = ", xvar_name(), yv, vl, ho, xl, yl, ml, nw,")")
                  return(lns)
            })
            
            output$text1 <- renderText({
            
              lns0 <- paste0("<hr><h3>Using Resma3</h3>")
              lns <- paste0("<h4>",fun(),"<p>")   
              nw <- ""
              if(yvar_name() != "")
                  if(input$new_order[1] != "") 
                     nw <- paste0(", new_order = c(", new_order()[1], paste0(", ",new_order()[-1],collapse=""),")")

              if(yvar_name() == "") lns[2] <- paste0("<br>stat.table(",xvar_name(),", ndigit = ",input$ndigit,")")
              else lns[2] <- paste0("<br>stat.table(",xvar_name(),", ",yvar_name(),nw,", ndigit = ",input$ndigit,")")
                  
              if(yvar_name() == "") lns[3] <- paste0("<br>stat.table(",xvar_name(),", Mean = FALSE, ndigit = ",input$ndigit,")")
              else lns[3] <- paste0("<br>stat.table(",xvar_name(),", ",yvar_name(),nw,", Mean = FALSE, ndigit = ",input$ndigit,")")

              if(yvar_name() == "") lns[4] <- paste0("<br>fivenumber(",xvar_name(),", ndigit = ",input$ndigit,")")
              else lns[4] <- paste0("<br>fivenumber(",xvar_name(),", ",yvar_name(),nw,", ndigit = ",input$ndigit,")")
              
                 
              lns[length(lns)] <- paste(lns[length(lns)],"</h4><hr>",sep="") 
              c(lns0,lns)     
                    
            }) 
            
            output$text2 <- renderText({
              if(wrns[2] != "") return("")
              lns <- "<hr><h3>Using ggplot2 (for graph)</h3><p><h4>"
              if(yvar_name() == "") 
                   lns[length(lns)+1] <- paste0("dta <- data.frame(y = ",xvar_name(),", x = rep(\" \", length(",xvar_name(),")))")
              else     
                  lns[length(lns)+1] <- paste0("dta <- data.frame(y = ",xvar_name(),", x = ",yvar_name(),")")
              if(input$whichgraph == "Boxplot")
                  lns[length(lns)+1] <- "<br>plt <- ggplot(aes(x, y), data = dta) + geom_boxplot()"  
              else
                  lns[length(lns)+1] <- "<br>plt <- ggplot(aes(x, y), data = dta) + geom_violin()"   
              if(input$orientation =="Horizontal") 
                    lns[length(lns)+1] <- "<br>plt <- plt + coord_flip()"      
 
              if(input$lbls == "Yes") {
                    if(input$label_x != "") 
                        lns[length(lns)+1] <- paste0("<br>plt <- plt + xlab(\"",input$label_x,"\")")
                    if(yvar_name() != "") 
                        if(input$label_y != "")  
                            lns[length(lns)+1] <-  paste0("<br>plt <- plt + ylab(\"",input$label_y,"\")")
                    if(input$main_title != "") 
                        lns[length(lns)+1] <-  paste0("<br>plt <- plt + ggtitle(\"",input$main_title,"\") +
                                theme(plot.title = element_text(size=20, colour= \"blue\" ))")
              }
              
              lns[length(lns)+1] <- "<br>print(plt)</h4><hr>"
              lns    
            })  

            
            output$text3 <- renderText({
              if(wrns[2] != "") return("")
              funs <- c("length", "mean", "median", "min", "max", "quantile")
              xtra <- c(rep("",5),", probs = c(0.25, 0.75)")
              f1 <- function(x, rna = "") {
                  if(x == "quantile") 
                      paste0("<br>", x,"(", vars[1], rna, ", probs = c(0.25, 0.75))")
                  else
                      paste0("<br>", x,"(", vars[1], rna, ")")
              }
              f2 <- function(x, rna = "") {
                  if(x == "quantile")
                      paste0("<br>tapply(", vars[1], ", ", vars[2], ", ",x , rna, ", probs = c(0.25, 0.75))")
                  else  
                      paste0("<br>tapply(", vars[1], ", ", vars[2], ", ",x , rna, ")")
              }
              lns <- "<hr><h3>Using R (for summary statistics)</h3><p><h4>"
              rna <- ifelse(any(is.na(df[, xvar_name()])), ", na.rm = TRUE", "")
              vars <- c(xvar_name(), yvar_name())
              lns0 <- paste0("<h4>")    
              if(yvar_name() == "") lns <- sapply(funs, f1, rna = rna)              
              else lns <- sapply(funs, f2, rna = rna)
              c("<h3>Using R (for summary statistics)</h3><p><h4>",lns,"</h4>")
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
