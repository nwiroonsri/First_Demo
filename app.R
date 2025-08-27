#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
data(AirPassengers)

# UI ----

ui <- fluidPage(
  
  # App title ----
  titlePanel("Clustering via Cluster Validity Indices by Nathakhun Wiroonsri"),
  sidebarLayout(
    
    sidebarPanel(width = 3,
                 selectInput(inputId = "clus_alg",
                             label = "Select Clustering Algorithm",
                             choices = c("K-Means", "Hierarchical Clustering", "Fuzzy C-Means"),
                             selected = "K-Means"),
                 # Linear Regression model arguments
                 conditionalPanel(condition = "input.clus_alg == 'K-Means'",
                                  checkboxGroupInput(inputId = "KM_args", 
                                                     label = "Select Clustering Features:", 
                                                     choices = list("Traditional" = 1, 
                                                                    "Bayesian" = 2),
                                                     selected = 1),
                                  numericInput(inputId = "kmax",
                                              label = "Kmax:",
                                              min = 2,
                                              max = 20,
                                              value = 10,
                                              step = 1),
                                  sliderInput(inputId = "k1",
                                              label = "1st Preferred K (<= Kmax)",
                                              min = 2,
                                              max = 20,
                                              value = c(2,3),
                                              step = 1),
                                  sliderInput(inputId = "kl1",
                                              label = "Level of Preference",
                                              min = 1,
                                              max = 3,
                                              value = 3,
                                              step = 1),
                                  sliderInput(inputId = "k2",
                                              label = "2nd Preferred K (<= Kmax)",
                                              min = 2,
                                              max = 20,
                                              value = c(2,3),
                                              step = 1),
                                  sliderInput(inputId = "kl2",
                                              label = "Level of Preference",
                                              min = 1,
                                              max = 3,
                                              value = 2,
                                              step = 1),
                                  sliderInput(inputId = "k3",
                                              label = "3rd Preferred K (<= Kmax)",
                                              min = 2,
                                              max = 20,
                                              value = c(2,3),
                                              step = 1),
                                  sliderInput(inputId = "kl3",
                                              label = "Level of Preference",
                                              min = 1,
                                              max = 3,
                                              value = 1,
                                              step = 1)
                                  ),
                 # ARIMA model arguments
                 conditionalPanel(condition = "input.model == 'ARIMA'",
                                  h5("Order Parameters"),
                                  sliderInput(inputId = "p",
                                              label = "p:",
                                              min = 0,
                                              max = 5,
                                              value = 0),
                                  sliderInput(inputId = "d",
                                              label = "d:",
                                              min = 0,
                                              max = 5,
                                              value = 0),
                                  sliderInput(inputId = "q",
                                              label = "q:",
                                              min = 0,
                                              max = 5,
                                              value = 0),
                                  h5("Seasonal Parameters:"),
                                  sliderInput(inputId = "P",
                                              label = "P:",
                                              min = 0,
                                              max = 5,
                                              value = 0),
                                  sliderInput(inputId = "D",
                                              label = "D:",
                                              min = 0,
                                              max = 5,
                                              value = 0),
                                  sliderInput(inputId = "Q",
                                              label = "Q:",
                                              min = 0,
                                              max = 5,
                                              value = 0)
                 ),
                 # Holt Winters model arguments
                 conditionalPanel(condition = "input.model == 'Holt-Winters'",
                                  checkboxGroupInput(inputId = "hw_args", 
                                                     label = "Select Holt-Winters Parameters:", 
                                                     choices = list("Beta" = 2, 
                                                                    "Gamma" = 3),
                                                     selected = c(1, 2, 3)),
                                  selectInput(inputId = "hw_seasonal",
                                              label = "Select Seasonal Type:",
                                              choices = c("Additive", "Multiplicative"),
                                              selected = "Additive")),
                 
                 checkboxInput(inputId = "log", 
                               label = "Log Transformation",
                               value = FALSE),
                 sliderInput(inputId = "h",
                             label = "Forecasting Horizon:",
                             min = 1,
                             max = 60,
                             value = 24)
                 #   actionButton(inputId = "update",
                 #                 label = "Update!")
                 
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(width = 9,
              # Forecast Plot ----
              plotOutput(outputId = "fc_plot",
                         height = "400px")
              
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  kl1 <- c("Low", "Medium", "High")
  
  output$kl1 <- renderText({
    paste("You selected:", kl1[input$kl1])
  })
  
  # Load the dataset a reactive object
  d <- reactiveValues(df = data.frame(input = as.numeric(AirPassengers), 
                                      index = seq.Date(from = as.Date("1949-01-01"),
                                                       by = "month",
                                                       length.out = length(AirPassengers))),
                      air = AirPassengers)
  
  # Log transformation 
  observeEvent(input$log,{
    if(input$log){
      d$df <- data.frame(input = log(as.numeric(AirPassengers)), 
                         index = seq.Date(from = as.Date("1949-01-01"),
                                          by = "month",
                                          length.out = length(AirPassengers)))
      
      d$air <- log(AirPassengers)
    } else {
      d$df <- data.frame(input = as.numeric(AirPassengers), 
                         index = seq.Date(from = as.Date("1949-01-01"),
                                          by = "month",
                                          length.out = length(AirPassengers)))
      
      d$air <- AirPassengers
    }
  })
  
  # The forecasting models execute under the plot render
  output$fc_plot <- renderPlot({
    
    # if adding a prediction intervals level argument set over here
    pi <- 0.95
    
    # Holt-Winters model
    if(input$model == "Holt-Winters"){
      a <- b <- c <- NULL
      
      if(!"2" %in% input$hw_args){
        b <- FALSE
      }
      
      if(!"3" %in% input$hw_args){
        c <- FALSE
      }
      
      md <- HoltWinters(d$air, 
                        seasonal = ifelse(input$hw_seasonal == "Additive", "additive", "multiplicative"),
                        beta = b,
                        gamma = c
      )
      fc <- predict(md, n.ahead = input$h, prediction.interval = TRUE) |>
        as.data.frame()
      fc$index <- seq.Date(from = as.Date("1961-01-01"),
                           by = "month",
                           length.out = input$h)
      # ARIMA model
    } else if(input$model == "ARIMA"){
      
      md <- arima(d$air,
                  order = c(input$p, input$d, input$q),
                  seasonal = list(order = c(input$P, input$D, input$Q))
      )
      fc <- predict(md, n.ahead = input$h, prediction.interval = TRUE) |>
        as.data.frame() 
      names(fc) <- c("fit", "se")
      
      fc$index <- seq.Date(from = as.Date("1961-01-01"),
                           by = "month",
                           length.out = input$h)
      
      fc$upr <- fc$fit + 1.96 * fc$se
      fc$lwr <- fc$fit - 1.96 * fc$se
      # Linear Regression model
    } else if(input$model == "Linear Regression"){
      
      d_lm <- d$df
      
      d_fc <- data.frame(index = seq.Date(from = as.Date("1961-01-01"),
                                          by = "month",
                                          length.out = input$h))
      
      if("1" %in% input$lm_args){
        d_lm$trend <- 1:nrow(d_lm)
        d_fc$trend <- (max(d_lm$trend) + 1):(max(d_lm$trend) + input$h)
      }
      
      if("2" %in% input$lm_args){
        d_lm$season <- as.factor(months((d_lm$index)))
        d_fc$season <- factor(months((d_fc$index)), levels = levels(d_lm$season))
      }
      
      md <- lm(input ~ ., data = d_lm[, - which(names(d_lm) == "index")])
      
      fc <- predict(md, n.ahead = input$h, interval = "prediction",
                    level = pi, newdata = d_fc) |>
        as.data.frame() 
      
      
      fc$index <- seq.Date(from = as.Date("1961-01-01"),
                           by = "month",
                           length.out = input$h)
      
    }
    
    # Setting the plot
    at_x <- pretty(seq.Date(from = min(d$df$index),
                            to = max(fc$index),
                            by = "month"))
    
    at_y <- c(pretty(c(d$df$input, fc$upr)), 1200)
    
    plot(x = d$df$index, y = d$df$input,
         col = "#1f77b4",
         type = "l",
         frame.plot = FALSE,
         axes = FALSE,
         panel.first = abline(h = at_y, col = "grey80"),
         main = "AirPassengers Forecast",
         xlim = c(min(d$df$index), max(fc$index)),
         ylim = c(min(c(min(d$df$input), min(fc$lwr))), max(c(max(fc$upr), max(d$df$input)))),
         xlab = paste("Model:", input$model, sep = " "),
         ylab = "Num. of Passengers (in Thousands)")
    mtext(side =1, text = format(at_x, format = "%Y-%M"), at = at_x,
          col = "grey20", line = 1, cex = 0.8)
    
    mtext(side =2, text = format(at_y, scientific = FALSE), at = at_y,
          col = "grey20", line = 1, cex = 0.8)
    lines(x = fc$index, y = fc$fit, col = '#1f77b4', lty = 2, lwd = 2)
    lines(x = fc$index, y = fc$upr, col = 'blue', lty = 2, lwd = 2)
    lines(x = fc$index, y = fc$lwr, col = 'blue', lty = 2, lwd = 2)
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)