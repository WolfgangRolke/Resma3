idataio <-
function(dataset) {

  require(shiny)
  require(rhandsontable)
  require(rio)
  imprt <- ifelse(missing(dataset), TRUE, FALSE)    

  if(!imprt) {
    out <- runApp(list(
      ui = fluidPage(
        tags$head(
          tags$style( HTML("table, th, td { text-align:right; }
                                   th, td { padding: 10px;    }    
           ")),
           tags$style(type = "text/css","label { font-size: 20px; }"
           )
        ),
        titlePanel("Interactive Data Import/Export App"),
        wellPanel(
            actionButton("doneButton", HTML("<h4><font color=\"blue\">Close App</font></h4>"))
        ),
        wellPanel(
            textInput("file", "Filename (with extension)", value = ""),
            uiOutput("folder")
        )
      ),
      server = function(input, output,session) {
         observe({
           if(input$doneButton > 0) {
               saveit()
               stopApp(NULL)
           }   
        })   
        
        output$folder <- renderUI({
            textInput("folder", "Folder", value = getwd())
        
        })   
        
        saveit <- reactive({
            if(input$folder == "") all <- paste0(getwd(),"/",input$file)
            all <- paste0(input$folder,"/",input$file)
            export(dataset, all)
        })
              
      }
     ))
     out
  } else {  
    out <- runApp(list(
      ui = fluidPage(
        tags$head(
          tags$style( HTML("table, th, td { text-align:right; }
                                   th, td { padding: 10px;    }    
           ")),
           tags$style(type = "text/css","label { font-size: 20px; }"
           )
        ),
        titlePanel("Interactive Data Import/Export App"),
        wellPanel(
            actionButton("doneButton", HTML("<h4><font color=\"blue\">Close App</font></h4>"))
        ),
        wellPanel(
            radioButtons("origin", "Where is data?", 
               choices = c("Enter data with keyboard",
                          "Read data from file",
                          "Get data from Internet",
                          "Copy from clipboard"), inline = TRUE),
             textInput("flnm", "Name of data set in R", value="tmp"),
             uiOutput("saveittxt")       
        ),
        conditionalPanel( condition = "input.origin == 'Enter data with keyboard' |
                                       input.origin ==  'Copy from clipboard'",
            wellPanel(
              fluidRow(
                column(2,numericInput("nc", "Number of Variables", value = 2, step = 1, min = 1)),            
                column(2,numericInput("nr", "Number of    Cases", value = 3, step = 1, min = 1)),        
                column(8,textInput("colnm", "Names of Variables (separate by empty space, uses letters if empty)", value=""))
              ),
              conditionalPanel( condition = "input.origin == 'Copy from clipboard'",
                radioButtons("hdr", "First row is variable names?", 
                     choices = c("Yes", "No"), inline = TRUE),
                div(HTML("<h5>Select the number of variables and the number of cases of the data set,
                      select No if the data has no variable names, go to data set and copy it 
                      to the clipboard 
                      if you have not done so yet, and then click on GO!</h5>")),     
                actionButton("goButton1", HTML("<h5><font color=\"red\">GO!</font></h5>"))     
              )  
            )
        ),    
        conditionalPanel( condition = "input.origin == 'Read data from file'", 
            wellPanel(
              radioButtons("fullfile", "How?", choices = c("Enter path and file name", "Brouse ..."), inline = TRUE), 
                conditionalPanel( condition = "input.fullfile == 'Enter path and file name'",
                    textInput("filepath", "Enter here", value = "c:/"),
                    actionButton("getFileButton", HTML("<h5><font color=\"red\">click when ready!</font></h5>"))
                )
           )                
        ),
        conditionalPanel( condition = "input.origin == 'Get data from Internet'", 
            wellPanel(
                textInput("lnk", "Enter URL and file name, with file extension",
                value = "http://test.com/data.csv"),
                actionButton("goButton", HTML("<h5><font color=\"red\">Click when ready to get data</font></h5>")) 
            )
        ),    
        conditionalPanel( condition = "input.origin == 'Enter data with keyboard' |
                                      input.origin == 'Copy from clipboard'",       
            rHandsontableOutput("keytbl")
        ),  
        wellPanel(  
          selectInput("att", "Should data be attached upon closing app?", choices=c("No","Yes"))
        ),    
        uiOutput("showdata")
      ),
      server = function(input, output,session) {
          
            observe({
               if(input$doneButton > 0) {
                  saveit()
                  stopApp(data())
               }   
            })
            
            fileBrouse <- reactive({
                  if(input$origin != "Read data from file") return("")
                  if(input$fullfile != "Brouse ...") return("")
                  file.choose()
            })           

            filePath <- eventReactive(input$getFileButton,{
                  if(input$origin != "Read data from file") return("")
                  if(input$fullfile == "Brouse ...") return("")               
                  input$filepath
            })           

            
            fileLink <- eventReactive(input$goButton,{ 
                 input$lnk                               
            })
            
            copydata  <- eventReactive(input$goButton1,{ 
                 df <- matrix(scan("clipboard", what = "char"), ncol = input$nc, byrow = TRUE)
                 if(input$hdr == "Yes") {
                     colnames(df) <- df[1, ]
                     df <-df[-1, ]
                 }
                 else colnames(df) <- letters[1:as.numeric(input$nc)]                               
                 df
            })
            
            data <- reactive({
                            
              if(input$origin %in% c("Enter data with keyboard", "Copy from clipboard")) { 
                  out <- hot_to_r(input$keytbl)
              }    
              
              if(input$origin %in% c("Get data from Internet", "Read data from file")) {
                if(input$origin == "Get data from Internet") filename <- fileLink()
                else { 
                    if(input$fullfile == "Brouse ...") filename <- fileBrouse()
                    else filename <- filePath()
                 }                
                 out <- import(filename)        
              }  
              if(is.null(out)) return(1)
              for(i in 1:ncol(out)) {
                 if(is.character(out[, i]))
                      out[, i] <- type.convert(out[, i], as.is  = TRUE)
                 if(ncol(out) == 1) out <- c(out)
              } 
              if(input$nc == 1) out <- c(out)           
              out
            })
            
            output$showdata <- renderText({
              if(input$origin %in% c("Enter data with keyboard","Copy from clipboard")) 
                    return("")

              if(is.null(data())) return("")              
              htbl(head(data()))
            })
        
            output$keytbl <- renderRHandsontable({  
                df <- matrix("", as.numeric(input$nr), as.numeric(input$nc))
                colnames(df) <- letters[1:as.numeric(input$nc)]
                if(input$origin == "Copy from clipboard") {                
                    df <- copydata()                    
                }    
                if(input$colnm != "") {
                     a <- unlist(strsplit(input$colnm," "))
                     a <- a[a!=""]
                     colnames(df)[1:length(a)] <- a
                }     
                df <- data.frame(df)    
                rhandsontable(df , rowHeaders = NULL)
            })  
            
            output$saveittxt <- renderText({
                tmp <- paste0(getwd(), "/", input$flnm, ".R")
                paste0("<h4>When closing app data will be saved in
                directory ", getwd(), " as ", input$flnm, ".rds</h4>")
            })
            
            saveit <- reactive({
                tmp <- paste0(getwd(), "/", input$flnm, ".rds")
                dta <- data()
                saveRDS(dta, tmp)
                if(input$att == "yes") attach(dta)
                NULL
            })
            
      }       
            
   ))
   a <- out
   if(is.null(dim(a))) {
      a <- unlist(a)
      names(a) <- NULL
   }    
   return(a)
 }  
}
