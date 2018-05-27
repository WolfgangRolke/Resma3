binner <-
function(df) {
  require(shiny)
  shinyApp(
    ui = fluidPage(
      sidebarLayout(
          sidebarPanel(
              sliderInput("n", "Bins", 5, 100, 20)
          ),
          mainPanel(
              plotOutput("hist")
          )
    )), 
    server = function(input, output) {
      output$hist <- renderPlot({ 
        plt <- hplot(df, n = input$n,return.graph=TRUE)
        plt
      })
    }
  )
}
