ui <- fluidPage(
  
  # App title ----
  titlePanel("Clustering via Cluster Validity Indices by Nathakhun Wiroonsri"),
  sidebarLayout(
    
    sidebarPanel(width = 3,
                 selectInput(inputId = "Clustering Algorithm",
                             label = "Select Clustering Algorithm",
                             choices = c("K-Means", "Hierarchical Clustering", "Fuzzy C-Means"),
                             selected = "K-Means"),
                 # Linear Regression model arguments
                 conditionalPanel(condition = "input.model == 'K-Means'",
                                  checkboxGroupInput(inputId = "KM_args", 
                                                     label = "Select Regression Features:", 
                                                     choices = list("Traditional" = 1, 
                                                                    "Bayesian" = 2),
                                                     selected = 1),
                                  sliderInput(inputId = "kmax",
                                              label = "Kmax:",
                                              min = 2,
                                              max = 30,
                                              value = 2)),
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