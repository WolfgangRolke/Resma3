igetdata <-
function() {
    require(shiny)
    require(rhandsontable)
    require(readxl)
    fileformats <- c("Comma Delimited .csv", "Excel Worksheet .xlsx", "R .rds")
    
    out <- runApp(list(
      ui = fluidPage(
        tags$head(
          tags$style( HTML("table, th, td { text-align:right; }
                                   th, td { padding: 10px;    }    
                           "))
        ),
        titlePanel("Interactive Data Entry App"),
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
               fileInput('fileData', 'Upload file with data',
                           accept = c('.csv', '.tsv', '.xlsx', '.rds')),
               div(HTML("<h5>Currently supported file formats:
               Comma Delimited .csv, Excel Worksheet .xlsx and R .rds</h5>"))            
            )     
        ),
        conditionalPanel( condition = "input.origin == 'Get data from Internet'", 
            wellPanel(
                textInput("lnk", "Enter URL and file name, with file extension",
                value = "http://test.com/data.csv"),
                actionButton("goButton", HTML("<h5><font color=\"red\">Click when ready to get data</font></h5>")) 
            )
        ),    
        conditionalPanel( condition = "input.origin == 'Enter data with keyboard'",       
            rHandsontableOutput("keytbl")
        ),  
        wellPanel(  
          selectInput("att", "Should data be attached upon closing app?", choices=c("No","Yes"))
        ),    
        conditionalPanel( condition = "input.origin == 'Read data from file' |
                                       input.origin == 'Get data from Internet'",
            uiOutput("showdata")
        )

      ),
      server = function(input, output,session) {
          
            observe({
               if(input$doneButton > 0) {
                  saveit()
                  stopApp(data())
               }   
            })
            
            intdata <- eventReactive(input$goButton,{ 
                 tpe <- unlist(strsplit(input$lnk, ".", fixed = TRUE))
                 tpe <- tpe[length(tpe)]               
                 if( !(tpe %in% c("csv", "xlsx", "rds")) ) return(data.frame(0))
                 if(tpe == "csv")
                    out <- read.csv(url(input$lnk))
                 if(tpe == "xlsx")   
                    out <- read_excel(url(input$lnk))                 
                 if(tpe == "rds")   
                    out <- readRDS(url(input$lnk))                                          
                 out                                     
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
              
              if(input$origin == "Get data from Internet") {
                  out <- intdata()
              }
              
              if(input$origin %in% c("Enter data with keyboard", "Copy from clipboard")) { 
                  out <- hot_to_r(input$keytbl)
              }    
              
              if(input$origin == "Read data from file") {
                 inFile1 <- input$fileData
                 if (is.null(inFile1)) return(NULL)
                 tpe <- unlist(strsplit(as.character(inFile1[1]), ".", fixed = TRUE))
                 tpe <- tpe[length(tpe)]
                 if( !(tpe %in% c("csv", "xlsx", "rds")) ) return(dataframe(0))
                 
                 a <- data.frame(0)
                 if(tpe == "csv") {
                    a <- read.csv(inFile1$datapath, header = TRUE)
                    out <- data.frame(matrix(unlist(a), ncol = length(a)))          
                    colnames(out) <- names(a)   
                 }   
                 if(tpe == "xlsx") {
                    file.rename(inFile1$datapath,
                          paste(inFile1$datapath, ".xlsx", sep=""))
                          
                    out <- read_excel(paste0(inFile1$datapath, ".xlsx"), 1)
                 }
                 if(tpe == "rds")
                    out <- readRDS(inFile1$datapath)
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
              if(input$origin == "Enter data with keyboard") return("")
              if(input$origin == "Read data from file") {
                 inFile1 <- input$fileData
                 if (is.null(inFile1)) return(NULL)
                 tpe <- unlist(strsplit(as.character(inFile1[1]), ".", fixed = TRUE))                 
                 tpe <- tpe[length(tpe)]
                 if( !(tpe %in% c("csv", "xlsx", "rds")) )
                    return("<h2>this file format is currently not supported!!</h3>")
              }
              if(is.null(data())) return("")              
              htbl(head(data()))
            })
        
            output$keytbl <- renderRHandsontable({  
                df <- matrix("", as.numeric(input$nr), as.numeric(input$nc))
                colnames(df) <- letters[1:as.numeric(input$nc)]
                if(input$origin == "Copy from clipboard") 
                    df <- copydata()
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
   print(a)
   if(is.null(dim(a))) {
      a <- unlist(a)
      names(a) <- NULL
  }    
   a
}
