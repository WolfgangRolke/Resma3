itemplate <-
function(...) {
   out <- runApp(list(
        ui = fluidPage(
            tags$head(
              tags$style(HTML("
              table, th, td { text-align:right;}
              th, td {padding: 10px;}    
            "))
            ),
            titlePanel(" Replace.. "),
            sidebarLayout(
                sidebarPanel(
                  actionButton("doneButton", HTML("<h4><font color=\"blue\">Close App</font></h4>"))
                  
                ),
                mainPanel(
                  uiOutput("text"),
                  plotOutput("plt")
                )
           )   
        ), 
        server = function(input, output) {
            observe({
               if(input$doneButton > 0) 
                  stopApp(NULL)
            })
            
            output$text <- renderText({                   
              lns <- "<h5> Replace ... </h5>"

              lns
            })
                        
            output$plt <- renderPlot({             
                hist(rnorm(1000))
           })
        }
      ))
   out
}
