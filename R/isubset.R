isubset <-
function(...) {
    require(shiny)
    data <- arrange.arguments(x=list(...), y=match.call())
    df <- data[[1]]
    attach(df)
    wrns <- data[[4]]    
    namvar <- colnames(df)
    nvar <- ncol(df)
    df_name <- data[[3]]
    n_full <- nrow(df)
    islist <- c("equal to", "less then", "more than", "less or equal to", "more or equal to", "not equal to")
    whatsgn <- c("==", "<", ">", "<=", ">=", "!=")
    names(whatsgn) <- islist 
    
    out <- runApp(list(
      ui = fluidPage(
        wellPanel(
          titlePanel("Interactive Data Subsetting App"),
          uiOutput("textwrn"),
          helpText("App let's you find a subset of a data set based on up to 3 conditions. It also shows the
            R subset command that you can use directly in R")
        ),
        wellPanel(
          actionButton("doneButton", HTML("<h4><font color=\"blue\">Close App</font></h4>")),
          actionButton("goButton", HTML("<h4><font color=\"red\">Click when ready to run</font></h4>"))        
        ), 
        wellPanel(  
          radioButtons("own","Type in your own condition?", choices=c("No","Yes"), inline = TRUE),
          conditionalPanel( condition = "input.own=='Yes'",
            textInput("owncond", "Enter it here:",value="")
        )),
        conditionalPanel( condition = "input.own=='No'",
          wellPanel(
            radioButtons("conditions", "Select Number of Condition(s)", choices=1:3, inline = TRUE),
             fluidRow(
              column(2, selectInput("var11", "Variable", choices=c("",namvar))),
              column(3, selectInput("which1", "Condition",choices=islist)),
              column(3, selectInput("var12b", "", choices = c("Value",namvar))),
              conditionalPanel( condition = "input.var12b == 'Value'",
                   column(3, textInput("var12a", "Enter Value", value="0"))
              )
           ),
          conditionalPanel( condition = "input.conditions!='1'",
            selectInput("and_or12","Do you want",choices=c("Condition 1 AND Condition 2","Condition 1 OR Condition 2")),
            fluidRow(
              column(2, selectInput("var21","Variable",choices=c("",namvar))),
              column(3, selectInput("which2","Condition",choices=islist)),
              column(3, selectInput("var22b","",choices=c("Value",namvar))),
              conditionalPanel( condition = "input.var22b == 'Value'",
                   column(3, textInput("var22a","Enter Value",value="0"))
              )
            )        
          ),
          conditionalPanel( condition = "input.conditions=='3'",
            selectInput("and_or123","Do you want",choices=c("Conditions 1,2 AND Condition 3","Conditions 1,2 OR Condition 3")),
            fluidRow(
              column(2, selectInput("var31","Variable",choices=c("",namvar))),
              column(3, selectInput("which3","Condition",choices=islist)),
              column(3, selectInput("var32b","",choices=c("Value",namvar))),
              conditionalPanel( condition = "input.var32b == 'Value'",
                   column(3, textInput("var32a","Enter Value",value="0"))
              )
            )        
          ))
        ),
        conditionalPanel( condition = "input.own == 'No'",
          wellPanel(
            HTML("<hr> <h3>Condition:</h3><p>"),
            uiOutput("condition1text")
          )
        ),
        wellPanel(
          HTML("<hr> <h3>R Code</h3><p>"),
          uiOutput("code")
        ),
        HTML("<hr> <h3>Data</h3><p>"),
        uiOutput("nodata"),
        uiOutput("info"),
        tableOutput("data1"),
        tableOutput("data2")
      ), 
 
      server = function(input, output,session) {
          
            observe({
               if(input$doneButton > 0) 
                  stopApp(sub())
            })
            
            output$textwrn <- renderText({                   
              lns <- ""
              if(wrns[1] != "") lns <- paste0(lns,"<h3>Warning: ",wrns[1],"</h3><p>")
              lns
            })
                        
            getInfo <- eventReactive(input$goButton,{
                
                  if(input$own=="Yes") {
                      lns1 <- ""
                      lns3 <- input$owncond
                      lns2 <- paste0("<h4>subset(", df_name, ",",lns3,")</h4>")
                      return(list(lns1,lns2,lns3))
                  }
   
                  var12 <- ifelse(input$var12b == "Value", 
                      type.convert(input$var12a, as.is = TRUE), input$var12b)        
                  if(is.logical(var12)) var12 <- ifelse(var12, "TRUE", "FALSE")

                  if(!is.numeric(var12) & input$var12b == "Value") var12 <- paste0("\"",var12,"\"")
                  var22 <- ifelse(input$var22b=="Value", type.convert(input$var22a, as.is = TRUE), input$var22b)
                  if(is.logical(var22)) var22 <- ifelse(var22, "TRUE", "FALSE")

                  if(!is.numeric(var22) & input$var22b=="Value") var22 <- paste0("\"",var22,"\"")
                  var32 <- ifelse(input$var32b=="Value", type.convert(input$var32a, as.is = TRUE), input$var32b)
                  if(is.logical(var32)) var32 <- ifelse(var32, "TRUE", "FALSE")

                  if(!is.numeric(var32) & input$var32b=="Value") var32 <- paste0("\"",var32,"\"")
                  sgn1 <- whatsgn[input$which1]
                  sgn2 <- whatsgn[input$which2]
                  sgn3 <- whatsgn[input$which3]
                  A12 <- ifelse(input$and_or12 == "Condition 1 AND Condition 2", "AND", "OR")
                  B12 <- ifelse(input$and_or12 == "Condition 1 AND Condition 2", "&", "|")
                  A123 <- ifelse(input$and_or123 == "Conditions 1,2 AND Condition 3", "AND", "OR")
                  B123 <- ifelse(input$and_or123 == "Conditions 1,2 AND Condition 3", "&", "|")
                  
                  if(input$conditions=="1") {
                          lns1 <- paste("<h4>", input$var11, input$which1, var12,"</h4>")
                          lns3 <- paste(input$var11, sgn1, var12)
                          lns2 <- paste("<h4>subset(", df_name, ",",lns3,")</h4>", sep=" ")
                  }        
                  if(input$conditions=="2") {
                          lns1 <- paste("<h4>", input$var11, input$which1, var12,A12,
                                                input$var21, input$which2, var22,"</h4>")
                          lns3 <- paste(input$var11, sgn1, var12,B12,input$var21, sgn2, var22)
                          lns2 <- paste("<h4>subset(", df_name, ",",lns3,")</h4>", sep=" ")
                  }    
                  if(input$conditions=="3") {
                          lns1 <- paste("<h4>(", input$var11, input$which1, var12, A12,
                                                input$var21, input$which2, var22,")", A123,
                                                input$var31, input$which3, var32,"</h4>")
                          lns3 <- paste("(",input$var11, sgn1, var12, B12,
                                                input$var21, sgn2, var22,")",B123,
                                                input$var31, sgn3, var32)
                          lns2 <- paste("<h4>subset(", df_name, " ,",lns3,")</h4>", sep=" ")
                  }    

                  out<-list(lns1, lns2, lns3)
                  out
            })
            
            output$condition1text <- renderText({
                  getInfo()[[1]]
            })

            output$code <- renderText({
                  getInfo()[[2]]
            })
            
            sub <- reactive({
                  if(input$var11 == "" & input$own == "No") return(df)
                  if(input$own == "Yes" & input$owncond == "") return(df)
        
                   subset(df,eval(parse(text=getInfo()[[3]])))
                  
            })
            
            output$info <- renderText({
                  lns <- paste0("<h4>Data set has ",n_full," rows</h4>")
                  k <- ifelse(nvar==1,length(sub()),nrow(sub()))
                  if(k==n_full) return(lns)
                  c(lns, paste0("<br><h4>After substetting data set has ",k," rows</h4><p>"))  
            })
                  
            output$nodata <- renderText({
                  out <- sub()
                  if(length(rownames(out))==0) return("<h3>Condition results in no data!</h3>")
                  if(nvar==1 & length(out)==0) return("<h3>Condition results in no data!</h3>")
                  else return("")
            })           
            
            output$data1 <- renderTable ({
                 out <- sub() 
                 if(nvar==1) {
                      if(length(out)==0) return("")
                      else  out <- cbind(out)
                 }     
                 else if(length(rownames(out))==0) return("")                 
                 k <- nrow(out)
                 out <- cbind(Row=1:k,out)
                 if(k>20) out<-out[1:10,  ]
                 out
            })
            output$data2 <- renderTable ({
                 out <- sub()
                 if(nvar==1) {
                      if(length(out)==0) return("")
                      else  out <- cbind(out)
                 }
                 else if(length(rownames(out))==0) return("")
                 k <- nrow(out)
                 out <- cbind(Row=1:k,out)                 
                 if(k<=20) return("") 
                 out<-out[(k-9):k,  ]
                 out
            })
            
     }       
            
   ))
   detach(df)
   out
}
