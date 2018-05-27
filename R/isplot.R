isplot <-
function(df) {
    require(shiny)
    
    numVars<-1
    if(any(c(is.matrix(df),is.data.frame(df)))) {
        catVar <- rep(FALSE,dim(df)[2])
        for(i in 1:dim(df)[2]) catVar[i] <- !is.numeric(df[, i])
        numVars <- colnames(df)[!catVar]
    }
    df_name <- deparse(substitute(df))
    shinyApp(
      ui = fluidPage(
        tags$head(
      tags$style(HTML("
          table, th, td {
              text-align:right;
          }
          
          th, td {
              padding: 10px;
          }    
       "))
  ),
  titlePanel("Interactive Scatterplot App"),
        sidebarLayout(
          sidebarPanel(
            actionButton("doneButton", HTML("<h4><font color=\"blue\">Close App</font></h4>")),
            selectInput("varx","X Variable",choices=c("",numVars)),
            uiOutput("listy"),
            selectInput("varz","Grouping Variable(s)",choices=c("None",colnames(df)),selected="None"),
            conditionalPanel( condition = "input.varz != 'None'",
                selectInput("varw","Second Grouping Variable",choices=c("None",colnames(df)),selected="None"),
                conditionalPanel( condition = "input.varw == 'None'",
                   selectInput("col_fac","Show How?",choices=c("With Color","With Facets"))           
                ),
                selectInput("noLeg","Eliminate all Legends(s)?",choices=c("No","Yes"))   
            ),            
            selectInput("fit","Add Fitted Line?",choices=c("No","Yes")),
            conditionalPanel( condition = "input.fit == 'Yes'",
                selectInput("mod","Type of Fit",choices=c("Linear","Nonparametric")),
                selectInput("err","With Error Bands?",choices=c("No","Yes"))
            ),
            selectInput("jttr","Jitter Dots?",choices=c("No","Yes")),  
            selectInput("lbls","Add / Change Labels?",choices=c("No","Yes")),  
            conditionalPanel( condition = "input.lbls == 'Yes'",
                textInput("label_x","X Axis Label",value=""),
                textInput("label_y","Y Axis Label",value=""),
                conditionalPanel( condition = "input.varz != 'None'",
                    textInput("label_z","Legend Label",value="")
                ),    
                textInput("main_title","Title",value="")
            ),
            selectInput("chngepltsym","Change Symbols/Size/Colors?",choices=c("No","Yes")),  
            conditionalPanel( condition = "input.chngepltsym == 'Yes'",  
                textInput("plotting.symbols","Symbol(s)",value=""), 
                textInput("plotting.colors","Color(s)",value=""),
                numericInput("plotting.size","Size",value=1)
            ),
            selectInput("refln","Add Reference Lines?",choices=c("No","Yes")),  
            conditionalPanel( condition = "input.refln == 'Yes'",  
                textInput("ref_y","Horizontal",value=""), 
                textInput("ref_x","Vertical",value="")
            ),
            selectInput("addT","Add Text to Graph?",choices=c("No","Yes")),  
            conditionalPanel( condition = "input.addT == 'Yes'",
                textInput("add.text","Text (space seperator)",value=""),
                textInput("add.text_x","x coordinates",value=""),
                textInput("add.text_y","y coordinates",value="")
            ),
            selectInput("logscale","Use Log Scale?",choices=c("No","Yes")),
            conditionalPanel( condition = "input.logscale == 'Yes'",
                selectInput("whichaxeslog","Which Axes?",choices=c("x","y","both"))
            ),
            selectInput("doclick","Identify Observations?",choices=c("No","Yes")), 
            width=4            
          ),
          mainPanel(
                tabsetPanel(
                  tabPanel("Introduction",value="p1",uiOutput("introtext")),
                  tabPanel("ScatterPlot",value="p2",
                      conditionalPanel( condition = "input.varx != '' ,#& input.vary != ''", 
                          uiOutput("doplt"),
                          uiOutput("clicktext")
                      )    
                  ),
                  tabPanel("Symbol and Color Codes",value="p3",
                          plotOutput("pltSymbols",width = 450, height = 450),
                          uiOutput("textSymbols")
                  ),
                  tabPanel("Code",value="p4",
                      uiOutput("Resmatext"),
                      uiOutput("ggtext")
                  ), id="mytabs"
                )  
          )
        )  
      ), 
      server = function(input, output,session) {
            output$listy <- renderUI({
                lst <- ""
                if(input$varx!="") lst <- numVars[numVars!=input$varx]
                if(length(numVars)>2) lst <- c("",lst) 
                selectInput("vary","Y Variable", choices=lst)
            })
            
            output$doplt <- renderUI({
                if(input$doclick=="No")
                    out <- plotOutput("plt",width = 450, height = 450)
                else
                    out <-  plotOutput("plt",width = 450, height = 450,
                            hover = "plot1_click")
                out    
            })            
            
            
          
            observe({
               if(input$doneButton > 0) stopApp()
            })
            observe({
                if (input$varx=="") {
                    updateTabsetPanel(session, "mytabs", selected = "p1")
                } else {
                  updateTabsetPanel(session, "mytabs", selected = "p2")
                }
            })
            makeps <- reactive({
                 inp_to_txt <-function(z) {
                    out<-list(vec=NA,txt_w_q=NA,txt_wou_q=NA)
                    if(z=="") return(out)
                    a <- unlist(strsplit(z," "))
                    a <- a[a!=""]
                    tmp <- as.numeric(a)
                    if(any(is.na(tmp))) out[[1]] <- a
                    else out[[1]] <- tmp
                    if(length(a)==1) {
                        out[[2]] <- paste0("\"",a,"\"")
                        out[[3]] <- a
                    }    
                    else {
                        out[[2]] <- paste0("c(\"",paste0(a[1:(length(a)-1)],"\",\"",collapse=""),a[length(a)],"\")")
                        out[[3]] <- paste0("c(",paste0(a[1:(length(a)-1)],",",collapse=""),a[length(a)],")")
                    }
                    out
                 }               
                 list(plotting.symbols = inp_to_txt(input$plotting.symbols),
                      plotting.colors = inp_to_txt(input$plotting.colors),
                      ref_x = inp_to_txt(input$ref_x),
                      ref_y = inp_to_txt(input$ref_y),
                      add.text = inp_to_txt(input$add.text),
                      add.text_x = inp_to_txt(input$add.text_x),
                      add.text_y = inp_to_txt(input$add.text_y)                                            
                 )
            })
            
            output$introtext <- renderText({
                lns <- "<h2><font color=\"red\">Welcome to the Interactive Scatterplot App!</h2></font><p>"
                
                lns[2] <- "<h4>this app allows you to make beautiful scatterplots with the click of a few buttons.</h4><br>"
                lns[length(lns)+1] <- "<h4>To start, choose the x and y variable from the dropdown boxes on the left</h4><br>" 
                if(!any(c(is.matrix(df),is.data.frame(df),!length(numVars)<2))) { 
                      lns[length(lns)+1] <- "<h3><font color=\"red\">Data has to be a matrix or dataframe with at least two numeric columns!!!</h3></font><p>"
                      return(lns)
                }   
                lns[length(lns)+1] <- "<h4></h4><br>" 
                lns[length(lns)+1] <- "<h4></h4><br>" 
                lns[length(lns)+1] <- "<h4></h4><br>" 
                lns[length(lns)+1] <- "<h4></h4><br>" 
                
                lns
            })
            
            output$clicktext <- renderText({
                 if(input$varx == "" | input$vary == "" ) return("")
                 if(input$doclick=="Yes" & is.null(input$plot1_click)) return("<h4>Hover the mouse over an Observation</h4>") 
                 if(is.null(input$plot1_click)) return("") 
                 lns0 <- "<h4>Hover the mouse over an Observation</h4>"
                 dist <- (df[,input$varx]-input$plot1_click$x)^2+(df[,input$vary]-input$plot1_click$y)^2                  
                 k <- c(1:nrow(df))[dist==min(dist)]
                 tmp <- unlist(df[k, ])
                 lns <- "<table border>"
                 for(i in 1:length(tmp)) lns[i+1] <- paste0("<tr><td><h4>",colnames(df)[i],"</h4></td>
                                                                      <td><h4>",tmp[i],"</h4></td></tr>")
                 c(lns0,lns,"</table>")
           })
                  
            output$Resmatext <- renderText({
                plotting.symbols <- ifelse(is.numeric(makeps()$plotting.symbols[[1]][1]),makeps()$plotting.symbols[[3]],makeps()$plotting.symbols[[2]])  
                plotting.colors <- ifelse(is.numeric(makeps()$plotting.colors[[1]][1]),makeps()$plotting.colors[[3]],makeps()$plotting.colors[[2]])  
                lns0 <- c("<hr><h3>Using Resma3 - splot:</h3>",
                               paste0("<h4>attach(",df_name,") #if not already done</h4>")) 
                if(input$varx == "" | input$vary == "" ) return(lns0) 
                lns <- paste0("<h4>splot(",input$vary,", ",input$varx)  
                if(input$varz!="None") {
                      lns <- paste0(lns,", ",input$varz)
                      if(input$col_fac!="With Color" & input$varw=="None")
                          lns <- paste0(lns,", use.facets =TRUE")
                      if(input$varw!="None")    
                          lns <- paste0(lns,", ",input$varw)
                      if(input$noLeg=="Yes")    
                          lns <- paste0(lns,", noLegend=TRUE")
                }          
                if(input$fit=="Yes") {
                    if(input$mod=="Linear") lns <- paste0(lns,", add.line=1")
                    else lns <- paste0(lns,", add.line=2")
                    if(input$err=="Yes") lns <- paste0(lns,", errorbar=TRUE")
                } 
                if(input$lbls=="Yes") {   
                    if(input$label_x!="") lns <- paste0(lns,", label_x=\"",input$label_x,"\"")
                    if(input$label_y!="") lns <- paste0(lns,", label_y=\"",input$label_y,"\"")
                    if(input$label_z!="") lns <- paste0(lns,", label_z=\"",input$label_z,"\"")
                    if(input$main_title!="") lns <- paste0(lns,", main_title=\"",input$main_title,"\"")
                }            
                if(input$jttr=="Yes") lns <- paste0(lns,", jitter=TRUE")
                if(input$plotting.size!=1)  lns <- paste0(lns,", plotting.size=",input$plotting.size, sep="")
                if(input$plotting.symbols!="")  
                   lns <- paste0(lns,", plotting.symbols=",plotting.symbols)
                if(input$plotting.colors!="") 
                   lns <- paste0(lns,", plotting.colors=",plotting.colors)
                if(input$ref_x!="")  
                   lns <- paste0(lns,", ref_x=",makeps()$ref_x[[3]])
                if(input$ref_y!="")  
                   lns <- paste0(lns,", ref_y=",makeps()$ref_y[[3]])
                if(input$addT=="Yes")   
                   lns <- paste0(lns,", add.text=",makeps()$add.text[[2]],
                                     ", add.text_x=",makeps()$add.text_x[[3]],
                                     ", add.text_y=",makeps()$add.text_y[[3]])    
                if(input$logscale=="Yes") {
                    if(input$whichaxeslog %in% c("x","both"))
                      lns <- paste0(lns,", log_x = TRUE")                                 
                    if(input$whichaxeslog %in% c("y","both"))
                      lns <- paste0(lns,", log_y = TRUE")                                 
                }      
                lns <- paste0(lns,")</h4>")  
                c(lns0,lns)       
                
            })
             
            output$ggtext <- renderText({
                plotting.symbols <- ifelse(is.numeric(makeps()$plotting.symbols[[1]][1]),makeps()$plotting.symbols[[3]],makeps()$plotting.symbols[[2]])  
                plotting.colors <- ifelse(is.numeric(makeps()$plotting.colors[[1]][1]),makeps()$plotting.colors[[3]],makeps()$plotting.colors[[2]])  
                lns <- "<hr><h3>Using ggplot2:<p></h3><h4>"  
                if(input$varx == "" | input$vary == "") return(lns) 
                if(input$varz=="None" | input$col_fac!="With Color") {
                    if(input$plotting.symbols=="" & input$plotting.colors=="")   
                        lns[2] <- paste0("plt <- ggplot(aes(",input$varx,", ",input$vary,"), data = ",df_name,")
                                        + geom_point(size = ",input$plotting.size,")")
                    if(input$plotting.symbols!="" & input$plotting.colors=="") 
                        lns[2] <- paste0("plt <- ggplot(aes(",input$varx,", ",input$vary,"), data = ",df_name,")
                                        + geom_point(size = ",input$plotting.size,", shape = ",plotting.symbols,")")
                    if(input$plotting.symbols=="" & input$plotting.colors!="")   
                        lns[2] <- paste0("plt <- ggplot(aes(",input$varx,", ",input$vary,"), data = ",df_name,")
                                        + geom_point(size = ",input$plotting.size,", colour = ",plotting.colors,")")                    
                    if(input$plotting.symbols!="" & input$plotting.colors!="")   
                        lns[2] <- paste0("plt <- ggplot(aes(",input$varx,", ",input$vary,"), data = ",df_name,")
                                        + geom_point(size = ",input$plotting.size,", shape = ",plotting.symbols,", colour=",plotting.colors,")")                                        
                }
                else {
                    lns[2] <- paste0("plt <- ggplot(aes(",input$varx,", ",input$vary,", 
                                  color = factor(",input$varz,")), data = ",df_name,")
                               + geom_point(size = ",input$plotting.size,")")
                    if(input$plotting.symbols!="")   
                               lns[2] <- paste0(lns[2]," + scale_shape_manual(values=",plotting.symbols,")")
                    if(input$plotting.colors!="")   
                               lns[2] <- paste0(lns[2]," + scale_colour_manual(values=",plotting.colors,")")                    
                }
                if(input$fit=="Yes") {
                    lns[3] <- "<br>plt <- plt + geom_smooth(method = "
                    if(input$mod=="Linear")
                         lns[3] <- paste0(lns[3],"\"lm\"")                
                    else lns[3] <- paste0(lns[3],"\"loess\"")                     
                    if(input$err=="No")
                         lns[3] <- paste0(lns[3],", se = FALSE")                
                    lns[3] <- paste0(lns[3],")")                     
                }
                if(input$varz!="None" & input$col_fac!="With Color") 
                    lns[length(lns)+1] <- paste0("<br>plt <- plt +  facet_wrap(~",input$varz,")")                
                if(input$varw!="None") 
                    lns[length(lns)+1] <- paste0("<br>plt <- plt +  facet_wrap(~",input$varw,")")                                    
                if(input$label_x!="")
                    lns[length(lns)+1] <- paste0("<br>plt <- plt + xlab(\"",input$label_x,"\")")
                else lns[length(lns)+1] <- paste0("<br>plt <- plt + xlab(\"",input$varx,"\")")   
                if(input$label_y!="")
                     lns[length(lns)+1] <- paste0("<br>plt <- plt + ylab(\"",input$label_y,"\")")
                else lns[length(lns)+1] <- paste0("<br>plt <- plt + ylab(\"",input$vary,"\")")
                if(input$label_z!="")
                     lns[length(lns)+1] <- paste0("<br>plt <- plt + 
                            guides(color = guide_legend(title = \"",input$label_z,"\"))")
                else
                     lns[length(lns)+1] <- paste0("<br>plt <- plt + 
                            guides(color = guide_legend(title = \"",input$varz,"\"))")                            
                if(input$main_title!="")
                     lns[length(lns)+1] <- paste0("<br>plt <- plt + ggtitle(\"",input$main_title,"\") +
                                theme(plot.title = element_text(size=20,colour=\"blue\"))")                    
                if(input$jttr=="Yes")    
                     lns[length(lns)+1] <- paste0("<br>plt <- plt + geom_jitter()") 
                if(input$ref_x!="") 
                     lns[length(lns)+1] <- paste0("<br>plt <- plt +  geom_hline(yintercept=",makeps()$ref_y[[3]],", linetype=1)") 
                if(input$ref_y!="") 
                     lns[length(lns)+1] <- paste0("<br>plt <- plt +  geom_vline(xintercept=",makeps()$ref_x[[3]],", linetype=1)") 
                if(input$add.text!="")
                     lns[length(lns)+1] <- paste0("<br> plt <- plt + annotate(\"text\",x = ",makeps()$add.text_x[[3]],",  
                                                   y = ",makeps()$add.text_y[[3]],",  
                                                   label = ",makeps()$add.text[[2]],")")
                if(input$logscale=="Yes") {
                      if(input$whichaxeslog %in% c("x","both"))
                               lns[length(lns)+1] <- "<br> plt <- plt + scale_x_log10()"
                      if(input$whichaxeslog %in% c("y","both"))
                               lns[length(lns)+1] <- "<br> plt <- plt + scale_y_log10()"
                }                                   
                                                   
                if(input$noLeg=="Yes") 
                     lns[length(lns)+1] <- "<br> plt <- plt + theme(legend.position=\"none\")"     
                lns[length(lns)+1] <- "<br>print(plt)</h4><hr>"         
                lns        
            })
            
            output$plt <- renderPlot({
                if(input$varx == "" | input$vary == "" ) return(NULL) 
                plotting.symbols <- makeps()$plotting.symbols[[1]]
                plotting.colors <- makeps()$plotting.colors[[1]]              
                if(input$varz=="None") 
                    plt <- splot(df[, input$vary],df[, input$varx], 
                          plotting.size=input$plotting.size, plotting.symbols=plotting.symbols, plotting.colors=plotting.colors, return.graph=TRUE)    
                else {
                    if(input$varw=="None") 
                       plt <- splot(df[, input$vary],df[, input$varx],df[, input$varz], 
                          use.facets = ifelse(input$col_fac=="With Color",FALSE,TRUE),
                          noLegend = ifelse(input$noLeg=="No",FALSE,TRUE),                          
                          plotting.size=input$plotting.size, plotting.symbols=plotting.symbols, plotting.colors=plotting.colors, return.graph=TRUE) 
                    else
                       plt <- splot(df[, input$vary],df[, input$varx],df[, input$varz],df[, input$varw], 
                          noLegend = ifelse(input$col_fac=="With Color",FALSE,TRUE), 
                          plotting.size=input$plotting.size, plotting.symbols=plotting.symbols, plotting.colors=plotting.colors, return.graph=TRUE)                         
                    plt <- plt + guides(color = guide_legend(title = input$varz))+guides(shape = FALSE)                         
                }           
                plt <- plt + xlab(input$varx)+ylab(input$vary)
                if(input$fit=="Yes") {
                    doErr <- ifelse(input$err=="Yes",TRUE,FALSE)
                    if(input$mod=="Linear")
                        plt <- plt + geom_smooth(method = "lm", se = doErr)
                    if(input$mod=="Nonparametric")
                        plt <- plt + geom_smooth(method = "loess", se = doErr)                   
                }         
                if(input$lbls=="Yes") {
                    if(input$label_x!="") plt <- plt + xlab(input$label_x)
                    if(input$label_y!="") plt <- plt + ylab(input$label_y)
                    if(input$main_title!="") plt <- plt + ggtitle(input$main_title) +
                            theme(plot.title = element_text(size=20,colour="blue"))
                    if(input$label_z!="") plt <- plt + 
                        guides(color = guide_legend(title = input$label_z))
                }         
                if(input$jttr=="Yes") plt <- plt + geom_jitter()                
                if(input$ref_y!="") 
                      plt <- plt + geom_hline(yintercept=makeps()$ref_y[[1]],linetype=1)
                if(input$ref_x!="") 
                      plt <- plt + geom_vline(xintercept=makeps()$ref_x[[1]],linetype=1)
                if(input$add.text!="")
                      plt <- plt + annotate("text",x = makeps()$add.text_x[[1]], 
                                                   y = makeps()$add.text_y[[1]], 
                                                   label = makeps()$add.text[[1]])
                if(input$logscale=="Yes") {
                      if(input$whichaxeslog %in% c("x","both"))
                          plt <- plt + scale_x_log10()
                      if(input$whichaxeslog %in% c("x","both"))
                          plt <- plt + scale_y_log10()
                }                                   
                plt
            })
            
            output$textSymbols <- renderText({ 
                 c("<h4>For either color or symbol you can enter numbers or text",
                   "<br>In the chart below you can see which numbers correspond to which
                        symbols and colors",
                   "<br>other wise just type in symbol or letter you want",
                   "<br>likewise the color, for example red or blue",
                   "<br>you need as many as the are groups<h4>")     
            })
            
            output$pltSymbols <- renderPlot({ 
                 k<-3
                 a<-c(1:25,32:(32+k*25-1))
                 plt <- ggplot(aes(x, y), data = data.frame(x=rep(1:25,k+1),
                              y=rep(seq(1,0,length=k+1),each=25)))+geom_point(color=a,shape=a,size=5) + 
                        geom_text(aes(label=a),hjust=0, vjust=2,size=5) + 
                        ylim(c(-0.2,1.2))
                 plt 
           })            
      }
   )
}
