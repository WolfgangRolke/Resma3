inormal <-
function(...) {
   require(shiny)
   require(ggplot2)
   out <- runApp(list(
        ui = fluidPage(
            tags$head(
              tags$style(HTML("
              table, th, td { text-align:right;}
              th, td {padding: 10px;}    
            "))
            ),
            titlePanel(" Illustration of Normal Distribution "),
            sidebarLayout(
                sidebarPanel(
                  actionButton("doneButton", HTML("<h4><font color=\"blue\">Close App</font></h4>")),
                  sliderInput( "mu", HTML("&mu;"), min = 0, max = 100, step = 1, value = 0),
                  sliderInput( "sigma", HTML("&sigma;"), min = 0, max = 30, step = 1, value = 1),
                  radioButtons( "st", "Compare to Standard Normal?", choices = c( "Yes", "No"))
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
              lns <- "<h5> Move sliders for different normal curves </h5>"

              lns
            })
                        
            output$plt <- renderPlot({             
                A <- input$mu - 3*input$sigma    
                B <- input$mu + 3*input$sigma
                x <- seq( A, B, length = 250)
                y <- dnorm( x, input$mu, input$sigma )
                dta <- data.frame( x = x, y = y )
                plt <- ggplot( aes( x, y), data = dta) +
                      geom_line( aes( x, y ) , colour = "blue", size = 1.1, data = dta)
                if( input$st == "Yes") {
                  x <- seq( -3, 3, length = 250)
                  y <- dnorm( x )
                  dta <- data.frame( x = x, y = y )
                  plt <- plt + geom_line( aes( x, y ) , colour = "red", size = 1.1, data = dta)
                
                
                }      
                plt      
           })
        }
      ))
   out
}
