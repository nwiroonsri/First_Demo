#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(e1071)
library(mclust)
#library(ggplot2)
#library(UniversalCVI)
#library(BayesCVI)
#data(AirPassengers)
#R1_data <- UniversalCVI::R1_data
# UI ----
data("R1_data", package = "UniversalCVI")

Wvalid = function (x, kmax, kmin = 2, method = "kmeans", corr = "pearson", 
          nstart = 100, sampling = 1, NCstart = TRUE) 
{
  if (missing(x)) 
    stop("Missing input argument. A numeric data frame or matrix is required")
  if (missing(kmax)) 
    stop("Missing input argument. A maximum number of clusters is required")
  if (!is.numeric(kmax)) 
    stop("Argument 'kmax' must be numeric")
  if (kmax > nrow(x)) 
    stop("The maximum number of clusters for consideration should be less than or equal to the number of data points in dataset.")
  if (!any(method == c("kmeans", "hclust_complete", "hclust_average", 
                       "hclust_single"))) 
    stop("Argument 'method' should be one of 'kmeans', 'hclust_complete', 'hclust_average' or 'hclust_single'")
  if (!any(corr == c("pearson", "kendall", "spearman"))) 
    stop("Argument 'corr' should be one of 'pearson', 'kendall', 'spearman'")
  if (method == "kmeans") {
    if (!is.numeric(nstart)) 
      stop("Argument 'nstart' must be numeric")
  }
  if (!is.numeric(sampling)) 
    stop("Argument 'sampling' must be numeric")
  if (!(sampling > 0 & sampling <= 1)) 
    stop("'sampling' must be greater than 0 and less than or equal to 1")
  if (sampling == 1) {
    x = x
  }
  else {
    sample = sample(1:(nrow(x)), ceiling(nrow(x) * sampling), 
                    replace = FALSE)
    x = x[sample, ]
  }
  if (!is.logical(NCstart)) 
    stop("Argument 'NCstart' must be logical")
  dm = dim(x)
  d = as.vector(dist(x))
  crr = rep(0, kmax - kmin + 2)
  if (NCstart) {
    dtom = sqrt(rowSums((x - colMeans(x))^2))
    crr[1] = sd(dtom)/(max(dtom) - min(dtom))
  }
  if (startsWith(method, "hclust_")) {
    H.model = hclust(dist(x), method = sub("hclust_", "", 
                                           method))
  }
  if (kmin == 2) {
    lb = 2
  }
  else {
    lb = kmin - 1
  }
  for (k in lb:(kmax + 1)) {
    xnew = matrix(0, dm[1], dm[2])
    centroid = matrix(0, k, dm[2])
    if (method == "kmeans") {
      K.model = kmeans(x, k, nstart = nstart)
      cluss = K.model$cluster
      xnew = K.model$centers[cluss, ]
    }
    else if (startsWith(method, "hclust_")) {
      cluss = cutree(H.model, k)
      for (j in 1:k) {
        if (is.null(nrow(x[cluss == j, ])) | sum(nrow(x[cluss == 
                                                        j, ])) == 1) {
          centroid[j, ] = as.numeric(x[cluss == j, ])
        }
        else {
          centroid[j, ] = colMeans(x[cluss == j, ])
        }
      }
      xnew = centroid[cluss, ]
    }
    if (!all(seq(k) %in% unique(cluss))) 
      warning("Some clusters are empty.")
    d2 = as.vector(dist(xnew))
    crr[k - kmin + 2] = cor(d, d2, method = corr)
  }
  K = length(crr)
  NWI = ((crr[2:(K - 1)] - crr[1:(K - 2)])/(1 - crr[1:(K - 
                                                         2)]))/pmax(0, (crr[3:K] - crr[2:(K - 1)])/(1 - crr[2:(K - 
                                                                                                                 1)]))
  NWI2 = (crr[2:(K - 1)] - crr[1:(K - 2)])/(1 - crr[1:(K - 
                                                         2)]) - (crr[3:K] - crr[2:(K - 1)])/(1 - crr[2:(K - 1)])
  NWI3 = NWI
  if (max(NWI) < Inf) {
    if (min(NWI) == -Inf) {
      NWI3[NWI == -Inf] = min(NWI[is.finite(NWI)])
    }
  }
  if (max(NWI) == Inf) {
    NWI3[NWI == Inf] = max(NWI[is.finite(NWI)]) + NWI2[NWI == 
                                                         Inf]
    NWI3[NWI < Inf] = NWI[NWI < Inf] + NWI2[NWI < Inf]
    if (min(NWI) == -Inf) {
      NWI3[NWI == -Inf] = min(NWI[is.finite(NWI)]) + NWI2[NWI == 
                                                            -Inf]
    }
  }
  NWI = data.frame(cbind(k = kmin:kmax, NCI1 = NWI))
  NWI2 = data.frame(cbind(k = kmin:kmax, NCI2 = NWI2))
  NWI3 = data.frame(cbind(k = kmin:kmax, NCI = NWI3))
  crr = data.frame(cbind(k = (kmin - 1):(kmax + 1), NC = crr))
  my_list <- list(NC = crr, NCI = NWI3, NCI1 = NWI, NCI2 = NWI2)
  return(my_list)
}

B_Wvalid = function (x, kmax, method = "kmeans", corr = "pearson", nstart = 100, 
          sampling = 1, NCstart = TRUE, alpha = "default", mult.alpha = 1/2) 
{
  if (missing(x)) 
    stop("Missing input argument. A numeric data frame or matrix is required")
  if (missing(kmax)) 
    stop("Missing input argument. A maximum number of clusters is required")
  if (!is.numeric(kmax)) 
    stop("Argument 'kmax' must be numeric")
  if (kmax > nrow(x)) 
    stop("The maximum number of clusters for consideration should be less than or equal to the number of data points in dataset.")
  if (!any(method == c("kmeans", "hclust_complete", "hclust_average", 
                       "hclust_single"))) 
    stop("Argument 'method' should be one of 'kmeans', 'hclust_complete', 'hclust_average' or 'hclust_single'")
  if (!any(corr == c("pearson", "kendall", "spearman"))) 
    stop("Argument 'corr' should be one of 'pearson', 'kendall', 'spearman'")
  if (method == "kmeans") {
    if (!is.numeric(nstart)) 
      stop("Argument 'nstart' must be numeric")
  }
  if (!is.numeric(sampling)) 
    stop("Argument 'sampling' must be numeric")
  if (!(sampling > 0 & sampling <= 1)) 
    stop("'sampling' must be greater than 0 and less than or equal to 1")
  if (sampling == 1) {
    x = x
  }
  else {
    sample = sample(1:(nrow(x)), ceiling(nrow(x) * sampling), 
                    replace = FALSE)
    x = x[sample, ]
  }
  if (!is.logical(NCstart)) 
    stop("Argument 'NCstart' must be logical")
  if (!is.numeric(mult.alpha)) 
    stop("Argument 'mult.alpha' must be numeric")
  n = nrow(x)
  kmin = 2
  if (any(alpha %in% "default")) {
    alpha = rep(1, length(kmin:kmax))
  }
  if (length(kmin:kmax) != length(alpha)) 
    stop("The length of kmin to kmax must be equal to the length of alpha")
  adj.alpha = alpha * (n)^mult.alpha
  dm = dim(x)
  d = as.vector(dist(x))
  crr = rep(0, kmax - kmin + 2)
  if (NCstart) {
    dtom = sqrt(rowSums((x - colMeans(x))^2))
    crr[1] = sd(dtom)/(max(dtom) - min(dtom))
  }
  if (startsWith(method, "hclust_")) {
    H.model = hclust(dist(x), method = sub("hclust_", "", 
                                           method))
  }
  if (kmin == 2) {
    lb = 2
  }
  else {
    lb = kmin - 1
  }
  for (k in lb:(kmax + 1)) {
    xnew = matrix(0, dm[1], dm[2])
    centroid = matrix(0, k, dm[2])
    if (method == "kmeans") {
      K.model = kmeans(x, k, nstart = nstart)
      cluss = K.model$cluster
      xnew = K.model$centers[cluss, ]
    }
    else if (startsWith(method, "hclust_")) {
      cluss = cutree(H.model, k)
      for (j in 1:k) {
        if (is.null(nrow(x[cluss == j, ])) | sum(nrow(x[cluss == 
                                                        j, ])) == 1) {
          centroid[j, ] = as.numeric(x[cluss == j, ])
        }
        else {
          centroid[j, ] = colMeans(x[cluss == j, ])
        }
      }
      xnew = centroid[cluss, ]
    }
    if (!all(seq(k) %in% unique(cluss))) 
      warning("Some clusters are empty.")
    d2 = as.vector(dist(xnew))
    crr[k - kmin + 2] = cor(d, d2, method = corr)
  }
  K = length(crr)
  NWI = ((crr[2:(K - 1)] - crr[1:(K - 2)])/(1 - crr[1:(K - 
                                                         2)]))/pmax(0, (crr[3:K] - crr[2:(K - 1)])/(1 - crr[2:(K - 
                                                                                                                 1)]))
  NWI2 = (crr[2:(K - 1)] - crr[1:(K - 2)])/(1 - crr[1:(K - 
                                                         2)]) - (crr[3:K] - crr[2:(K - 1)])/(1 - crr[2:(K - 1)])
  NWI3 = NWI
  if (max(NWI) < Inf) {
    if (min(NWI) == -Inf) {
      NWI3[NWI == -Inf] = min(NWI[is.finite(NWI)])
    }
  }
  if (max(NWI) == Inf) {
    NWI3[NWI == Inf] = max(NWI[is.finite(NWI)]) + NWI2[NWI == 
                                                         Inf]
    NWI3[NWI < Inf] = NWI[NWI < Inf] + NWI2[NWI < Inf]
    if (min(NWI) == -Inf) {
      NWI3[NWI == -Inf] = min(NWI[is.finite(NWI)]) + NWI2[NWI == 
                                                            -Inf]
    }
  }
  CVI.dframe = data.frame(C = kmin:kmax, Index = NWI3)
  minGI = min(CVI.dframe[, "Index"])
  rk = (CVI.dframe[, "Index"] - minGI)/sum(CVI.dframe[, "Index"] - 
                                             minGI)
  nrk = n * rk
  ex = (adj.alpha + nrk)/(sum(adj.alpha) + n)
  var = ((adj.alpha + nrk) * (sum(adj.alpha) + n - adj.alpha - 
                                nrk))/((sum(adj.alpha) + n)^2 * (sum(adj.alpha) + n + 
                                                                   1))
  BCVI = data.frame(k = kmin:kmax, BCVI = ex)
  VarBCVI = data.frame(k = kmin:kmax, Var = var)
  colnames(CVI.dframe) = c("k", "NCI")
  WI.result = list(BCVI = BCVI, VAR = VarBCVI, Index = CVI.dframe)
  return(WI.result)
}

ui <- fluidPage(
  
  # App title ----
  titlePanel("Clustering via Cluster Validity Indices by Nathakhun Wiroonsri"),
  sidebarLayout(
    sidebarPanel(width = 3,
                 selectInput(inputId = "clus_alg",
                             label = "Select Clustering Algorithm",
                             choices = c("K-Means", "Hierarchical Clustering", "Fuzzy C-Means"),
                             selected = "K-Means"),
                 selectInput(inputId = "cvi_opt",
                             label = "Select CVI style",
                             choices = c("Traditional", "Bayesian"),
                             selected = "Traditional"),
                 # Kmeans + Traditional
                 conditionalPanel(condition = "input.clus_alg == 'K-Means' && input.cvi_opt == 'Traditional'",
                                  numericInput(inputId = "kmax",
                                               label = "Kmax:",
                                               min = 2,
                                               max = 20,
                                               value = 10,
                                               step = 1)
                                 ),
                 # Kmeans + Bayesian
                 conditionalPanel(condition = "input.clus_alg == 'K-Means' && input.cvi_opt == 'Bayesian'",
                                  numericInput(inputId = "kmax2",
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
                                              min = 0,
                                              max = 3,
                                              value = 1,
                                              step = 1),
                                  sliderInput(inputId = "k2",
                                              label = "2nd Preferred K (<= Kmax)",
                                              min = 2,
                                              max = 20,
                                              value = c(2,3),
                                              step = 1),
                                  sliderInput(inputId = "kl2",
                                              label = "Level of Preference",
                                              min = 0,
                                              max = 3,
                                              value = 0,
                                              step = 1),
                                  sliderInput(inputId = "k3",
                                              label = "3rd Preferred K (<= Kmax)",
                                              min = 2,
                                              max = 20,
                                              value = c(2,3),
                                              step = 1),
                                  sliderInput(inputId = "kl3",
                                              label = "Level of Preference",
                                              min = 0,
                                              max = 3,
                                              value = 0,
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
                 
                 actionButton(inputId = "pushstart", label = "Start")
                 
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: Plot of the requested variable against mpg ----
      plotOutput("fc_plot", width = "600px", height = "600px")
      
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  dat = R1_data[,-3]
  dats = scale(dat)
  
 

  # The forecasting models execute under the plot render

  
  
observeEvent(input$pushstart, {
  
   if (input$clus_alg == "K-Means" && input$cvi_opt == "Traditional"){
     ww = Wvalid(dats, kmax = input$kmax)
     res = kmeans(dat,ww$NCI[which.max(ww$NCI[,2]),1],nstart = 100)
      output$fc_plot <- renderPlot({
        cols = rainbow(input$kmax)
        plot(dat,col = cols[res$cluster])
      })
   } else if (input$clus_alg == "K-Means" && input$cvi_opt == "Bayesian"){
     aa = rep(1,input$kmax-1)
     aa[(input$k1[1]-1):(input$k1[2]-1)] = aa[(input$k1[1]-1):(input$k1[2]-1)] + 10*input$kl1*aa[(input$k1[1]-1):(input$k1[2]-1)]
     aa[(input$k2[1]-1):(input$k2[2]-1)] = aa[(input$k2[1]-1):(input$k2[2]-1)] + 10*input$kl2*aa[(input$k2[1]-1):(input$k2[2]-1)]
     aa[(input$k3[1]-1):(input$k3[2]-1)] = aa[(input$k3[1]-1):(input$k3[2]-1)] + 10*input$kl3*aa[(input$k3[1]-1):(input$k3[2]-1)]
     ww = B_Wvalid(dats, kmax = input$kmax, alpha = aa)
     res = kmeans(dat,ww$BCVI[which.max(ww$BCVI[,2]),1],nstart = 100)
     output$fc_plot <- renderPlot({
       cols = rainbow(input$kmax)
       plot(dat,col = cols[res$cluster]) 
     })
  }
})
  
  
  
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)