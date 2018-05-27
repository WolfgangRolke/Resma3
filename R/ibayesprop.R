ibayesprop <-
function() {
  require(shiny)
  out <- runApp(list(
        ui = fluidPage(
            tags$head(
              tags$style(HTML("
              table, th, td { text-align:right;}
              th, td {padding: 10px;}    
            "))
            ),
            titlePanel("Interactive Bayesian Calculator for Percentages"),
            sidebarLayout(
                sidebarPanel(
                  actionButton("doneButton", HTML("<h4><font color=\"blue\">Close App</font></h4>")),
                  HTML("<h4>Prior Distribution</h4>"),
                  radioButtons("ptype", "How do you want to specify the prior distribution?",
                      choices = c("Location and Range", "Beta prior", "Discrete Prior", "Enter your own function"),
                      selected = "Location and Range"),
                  HTML("<hr><h4>Data:</h4>"),
                  textInput("n", "Sample Size", value = ""),    
                  textInput("x", "Number of Successes", value = ""),
                  selectInput("lv", "Confidence Level", choices = c(68, 90, 95, 99), selected = 95)
                ),
                mainPanel(
                 wellPanel(
                  conditionalPanel( condition = "input.ptype == 'Enter your own function'",
                      textInput("funtxt", "Enter R code for prior", value = "1")
                  ),                 
                  conditionalPanel( condition = "input.ptype == 'Beta prior'",
                      sliderInput("alpha", HTML(" &alpha;"), min = 0 , max = 20, step = 0.5, value = 0.5),  
                      sliderInput("beta", HTML(" &beta;"), min = 0, max = 20, step = 0.5, value = 0.5)
                  ),
                  conditionalPanel( condition = "input.ptype == 'Location and Range'",
                      textInput("mu", "most likely value", value = "50"),
                      textInput("r", "likely range of values", value = "40")
                  ),                  
                  conditionalPanel( condition = "input.ptype == 'Discrete Prior'",
                      wellPanel(
                          HTML("<h4>Enter your best guess for the probability that the true percentage
                          is in the interval.</h4>"),
                          sliderInput("dr", "Choose likely range of percentage", 
                              min=0, max=100, step=10, value=c(0,100)),
                          fluidRow(
                            column(2, uiOutput("p1")),
                            column(2, uiOutput("p2")),
                            column(2, uiOutput("p3")),
                            column(2, uiOutput("p4")),
                            column(2, uiOutput("p5"))
                          ),
                          fluidRow(
                            column(2, uiOutput("p6")),
                            column(2, uiOutput("p7")),
                            column(2, uiOutput("p8")),
                            column(2, uiOutput("p9")),
                            column(2, uiOutput("p10"))
                          )
                      )
                  )
                 ), 
                  plotOutput("plt"),
                  wellPanel(  
                    uiOutput("txt1"),
                    uiOutput("txt2")
                  )
                )
           )   
        ), 
        server = function(input, output, session) {
        
            observe({
               if(input$doneButton > 0) 
                  stopApp(NULL)
            })
            
            rPIT <- function ( fun, n=1e4, A=0.1, B=99.9) {            
              m <- min(2 * n, 1000)
              x <- seq(A, B, length = m)  
              y <- fun(x)            
              z <- (x[2] - x[1])/6 * cumsum((y[-1] + 4 * y[-2] + y[-3]))
              z <- z/max(z)
              y <- c(0, z)
              xyTmp <- cbind(x, y)
              approx(xyTmp[, 2], xyTmp[, 1], runif(n))$y    
            }
            
            xy <- reactive({ 
                  gotdata <- TRUE
                  if(input$x == "") {x <- 0; gotdata <- FALSE}
                  else x <- as.numeric(input$x)
                  if(input$n == "") {n <- 1; gotdata <- FALSE}
                  else n <- as.numeric(input$n)
                  list(x = x, n = n, gotdata = gotdata)
            })
            
            beta <- reactive({
                 al <- c( (1-as.numeric(input$lv)/100)/2, 1-(1-as.numeric(input$lv)/100)/2,
                      1-as.numeric(input$lv)/100)
                 par <- c(input$alpha, input$beta, xy()$x+input$alpha, xy()$n-xy()$x+input$beta)      
                 prior <- function(x) dbeta(x/100, par[1], par[2]) 
                 posterior <- function(x) dbeta(x/100, par[3], par[4]) 
                 priorest <- round(100*par[1]/sum(par[1:2]), 1)
                 posteriorest <- round(100*par[3]/sum(par[3:4]), 1)                
                 priorlim <- round( 100*qbeta( al[1:2], par[1], par[2] ), 1) 
                 posteriorlim <- c(0,0) 
                 if(xy()$x == 0) 
                     posteriorlim <- c(0, round( 100*qbeta(1-al[3], par[3], par[4]), 1)) 
                 if(xy()$x == xy()$n) 
                     posteriorlim <- c(round( 100*qbeta( al[3], par[3], par[4] ), 1), 100) 
                 if(xy()$x > 0 &xy()$x < xy()$n )    
                    posteriorlim <-round( 100*qbeta( al[1:2], par[3], par[4] ), 1)
                             
                  list(prior = prior, posterior = posterior, priorest = priorest, 
                    posteriorest = posteriorest, priorlim = priorlim, posteriorlim = posteriorlim)
            }) 
            
            locrange <- reactive({
                al <- c( (1-as.numeric(input$lv)/100)/2, 1-(1-as.numeric(input$lv)/100)/2,
                        1-as.numeric(input$lv)/100)
                prior <- function(x) dnorm(x, as.numeric(input$mu), as.numeric(input$r)/4)/
                           diff(pnorm(c(0,100), as.numeric(input$mu), as.numeric(input$r)/4))              
                posterior <- function(x) {x}
                ef <- function(x) {x*prior(x)}
                priorest <- round(integrate(ef, 0, 100)$value, 1)
                priorlim <- round(quantile( rPIT( fun=prior), al[1:2] ), 1) 
                posteriorest <- 0
                posteriorlim <- c(0, 0)
                if( xy()$gotdata) {
                   mx <- 1
                   posterior <- function(p) {dbinom( xy()$x, xy()$n, p/100)*prior(p)/mx}
                   mx <- integrate(posterior, 0, 100)$value     
                   ef <- function(x) {x*posterior(x)}
                   posteriorest <- round(integrate(ef, 0, 100)$value, 1)
                   x <- rPIT( fun=posterior )
                   if(xy()$x == 0 | xy()$x == xy()$n) {
                      if(xy()$x == 0) 
                          posteriorlim <- c(0, round(quantile(x, 1-al[3] ), 1)) 
                      if(xy()$x == xy()$n) 
                          posteriorlim <- c(round(quantile(x, al[3] ), 1), 100)
                   }   
                   else posteriorlim <-round(quantile(x, al[1:2] ), 1)
                }
                list(prior = prior, posterior = posterior, priorest = priorest, 
                    posteriorest = posteriorest, priorlim = priorlim, posteriorlim = posteriorlim)
            }) 
            
            drange <- reactive({
                low <- input$dr[1] + c(0:9)*diff(input$dr)/10
                high <- input$dr[1] + c(1:10)*diff(input$dr)/10
                round( (low+high)/2, 1)
            })
            output$p1 <- renderUI({numericInput("p1", label = drange()[1], min = 0, value=1, step=1)})
            output$p2 <- renderUI({numericInput("p2", label = drange()[2], min = 0, value=1, step=1)})
            output$p3 <- renderUI({numericInput("p3", label = drange()[3], min = 0, value=1, step=1)})
            output$p4 <- renderUI({numericInput("p4", label = drange()[4], min = 0, value=1, step=1)})
            output$p5 <- renderUI({numericInput("p5", label = drange()[5], min = 0, value=1, step=1)})
            output$p6 <- renderUI({numericInput("p6", label = drange()[6], min = 0, value=1, step=1)})
            output$p7 <- renderUI({numericInput("p7", label = drange()[7], min = 0, value=1, step=1)})
            output$p8 <- renderUI({numericInput("p8", label = drange()[8], min = 0, value=1, step=1)})
            output$p9 <- renderUI({numericInput("p9", label = drange()[9], min = 0, value=1, step=1)})
            output$p10 <- renderUI({numericInput("p10", label = drange()[10], min = 0, value=1, step=1)})
            
            discrete.f <- reactive({
                  al <- c( (1-as.numeric(input$lv)/100)/2, 1-(1-as.numeric(input$lv)/100)/2,
                        1-as.numeric(input$lv)/100)
                  p <- as.numeric( c( input$p1, input$p2, input$p3, input$p4, input$p5,
                         input$p6, input$p7, input$p8, input$p9, input$p10))
                  step <- diff(input$dr)/10     
                  low <- input$dr[1] + c(0:9)*step
                  high <- input$dr[1] + c(1:10)*step
                  mid <- (low+high)/2              
                  prior <-  p/sum(p) 
                  priorest <- round(sum(mid*prior), 1) 
                  x <- sample(mid, size=1e4, replace=T, prob = prior)
                  priorlim <- round(quantile(x, al[1:2]), 1) 
                  posteriorest <- 0
                  posteriorlim <- c(0,0) 
                  posterior <- rep(0,10)     
                  if( xy()$gotdata) {
                      mx <- sum(dbinom( xy()$x, xy()$n, mid/100)*prior)
                      posterior <- dbinom( xy()$x, xy()$n, mid/100)*prior/mx
                      posteriorest <- round(sum(mid*posterior), 1)
                      x <- sample(mid, size=1e4, replace=T, prob = posterior)
                      if(xy()$x == 0 | xy()$x == xy()$n) {
                        if(xy()$x == 0) 
                          posteriorlim <- c(0, round(quantile(x, 1-al[3] ), 1)) 
                        if(xy()$x == xy()$n) 
                          posteriorlim <- c(round(quantile(x, al[3] ), 1), 100)
                     }   
                     else posteriorlim <-round(quantile(x, al[1:2] ), 1) 
                  }
                  list(mid = mid, step = step, prior = prior, priorest = priorest, priorlim = priorlim,
                        posterior = posterior, posteriorest = posteriorest, posteriorlim = posteriorlim)
            
            })
  
            specialfun <- reactive({
                 al <- c( (1-as.numeric(input$lv)/100)/2, 1-(1-as.numeric(input$lv)/100)/2,
                        1-as.numeric(input$lv)/100)            
                if(input$ptype == "Enter your own function") {
                    if( !grepl("x", input$funtxt) ) 
                        f1 <- function(x) rep(0.01, length(x))
                    else      
                      f1 <- function(x) { eval(parse(text=input$funtxt)) }
                }
                M <- integrate(f1, input$dr[1], input$dr[2])$value
                prior <- function(x) f1(x)/M           
                posterior <- function(x) {x}
                ef <- function(x) {x*prior(x)}
                priorest <- round(integrate(ef, 0, 100)$value, 1)
                priorlim <- round(quantile(rPIT( fun=prior ), al[1:2] ), 1) 
                posteriorest <- 0
                posteriorlim <- c(0, 0)
                if( xy()$gotdata) {
                   mx <- 1
                   posterior <- function(p) {dbinom( xy()$x, xy()$n, p/100)*prior(p)/mx}
                   mx <- integrate(posterior, 0, 100)$value     
                   ef <- function(x) {x*posterior(x)}
                   posteriorest <- round(integrate(ef, 0, 100)$value, 1)
                   x <- rPIT( fun=posterior)
                   if(xy()$x == 0 | xy()$x == xy()$n) {
                      if(xy()$x == 0) 
                          posteriorlim <- c(0, round(quantile(x, 1-al[3] ), 1)) 
                      if(xy()$x == xy()$n) 
                          posteriorlim <- c(round(quantile(x, al[3] ), 1), 100)
                   }   
                   else posteriorlim <-round(quantile(x, al[1:2] ), 1)
                }
                list(prior = prior, posterior = posterior, priorest = priorest, 
                    posteriorest = posteriorest, priorlim = priorlim, posteriorlim = posteriorlim)
            }) 
            
            prior <- reactive({
              funnames <- c("beta", "locrange", "discrete.f", "specialfun")
              inputnames <- c("Beta prior", "Location and Range", "Discrete Prior", "Enter your own function")
              fun <- funnames[which(input$ptype == inputnames)]
              get(fun)()$prior
            })

            posterior <- reactive({
              funnames <- c("beta", "locrange", "discrete.f", "specialfun")
              inputnames <- c("Beta prior", "Location and Range", "Discrete Prior", "Enter your own function")
              fun <- funnames[which(input$ptype == inputnames)]
              get(fun)()$posterior
            })
                        
            pointestimate <- reactive({
              freq <- 0
              if( xy()$gotdata) freq <- 100*as.numeric(input$x)/as.numeric(input$n)
              funnames <- c("beta", "locrange", "discrete.f", "specialfun")
              inputnames <- c("Beta prior", "Location and Range", "Discrete Prior", "Enter your own function")
              fun <- funnames[which(input$ptype == inputnames)]
              c(get(fun)()$priorest, get(fun)()$posteriorest, freq)
            })
            
            limits <- reactive({ 
              freqlim <- c(0,0)
              if( xy()$gotdata) 
                   freqlim <- round(100*as.numeric(prop.test(as.numeric(input$x), as.numeric(input$n), 
                        conf.level = as.numeric(input$lv)/100 )$conf.int), 1)
              funnames <- c("beta", "locrange", "discrete.f", "specialfun")
              inputnames <- c("Beta prior", "Location and Range", "Discrete Prior", "Enter your own function")
              fun <- funnames[which(input$ptype == inputnames)]
              c(get(fun)()$priorlim, get(fun)()$posteriorlim, freqlim)
            })
            
           output$txt1 <- renderText({
             sp <- "<font size=\"+1\">"
              lns <- paste0("<table>
                 <tr><th>", sp, "Prior Point Estimate : </th><th>", sp, pointestimate()[1], "%</th></tr>")
              if( xy()$gotdata) 
                lns <- paste0(lns, 
                    "<tr><th>", sp, "Bayesian Point Estimate : </th><th>", sp, pointestimate()[2], "%</th></tr>
                    <tr><th>", sp, "Frequentist Point Estimate : </th><th>", sp, pointestimate()[3], "%</th></tr>")
              lns <- paste(lns, "</table>")            
              lns 
            })

           output$txt2 <- renderText({
              sp <- "<font size=\"+1\">"
              lns <- paste0("<hr><table>
                 <tr><th>", sp, "Prior ", input$lv, "% Credible Interval : </th><th>", sp, "( ", 
                      limits()[1], "%, ", limits()[2], "% )</th></tr>")
              if( xy()$gotdata) 
                lns <- paste0(lns, 
                    "<tr><th>", sp, "Bayesian ", input$lv, "% Credible Interval : </th><th>", sp, "( ", 
                      limits()[3], "%, ", limits()[4], "% )</th></tr>
                    <tr><th>", sp, "Frequentist ", input$lv, "% Confidence Interval : </th><th>", sp, "( ", 
                    limits()[5], "%, ", limits()[6], "% )</th></tr>")
              lns <- paste(lns, "</table>")            
              lns 
            })
            
            output$plt <- renderPlot({  
                x <- seq(1, 99, length=250)
                which <- factor( rep( c("Prior", "Posterior"), each=250), ordered = TRUE)              
                if(input$ptype != "Discrete Prior") {
                  y1 <- prior()(x)
                  ymax <- max(c(y1, 0.02)) 
                  if( xy()$gotdata ) {
                     y2 <- posterior()(x)
                     dta <- data.frame(Percentage = c(x, x), Density = c( y1, y2), which = which)
                  }
                  else dta <- data.frame(Percentage = x, Density = y1)
                }  
                if( input$x == "" | input$n == "") {                     
                     if(input$ptype == "Discrete Prior") {                     
                         dta <- data.frame(x = discrete.f()$mid, y =  discrete.f()$prior)
                         plt <- ggplot(data = dta, aes(x, y)) +
                            geom_segment(aes(x = discrete.f()$mid, y = rep(0,10), 
                                xend = discrete.f()$mid, yend = discrete.f()$prior), colour = "blue")+
                            xlab("Percentage") + ylab("Prior Probability") + xlim(c(0,100)) 
                     }
                     else {
                        plt <- ggplot(dta, aes(Percentage, Density)) + 
                          geom_line(colour = "blue")  + ylab("Prior Density") + ylim(0, ymax)
                     } 
                }
                else {
                     if(input$ptype == "Discrete Prior") {
                        dta <- data.frame(x = c(discrete.f()$mid, discrete.f()$mid), 
                                           y =  c(discrete.f()$prior, discrete.f()$posterior))
                        dta1 <- data.frame(x = discrete.f()$mid, y = discrete.f()$prior)
                        dta2 <- data.frame(x = discrete.f()$mid+1, y =  discrete.f()$posterior)                   
                        plt <- ggplot(data = dta, aes(x, y)) +
                            geom_segment(aes(x = x, y = rep(0, 10), xend = x, yend = y), data = dta1, colour = "blue") +
                            geom_segment(aes(x = x, y = rep(0, 10), xend = x, yend = y), data = dta2, colour = "red") +
                            xlab("Percentage") + ylab("Prior and Posterior Probabilities") + xlim(c(0,100))                     }
                     else {
                        plt <- ggplot(dta, aes(Percentage, Density, colour = which)) +
                            geom_line() + guides(color = guide_legend(title =""))+
                            scale_color_manual(values=c("red","blue")) 
                     }
                }
                plt 
                
            })
      }      
      ))
   out
}
