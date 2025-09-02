library(shiny)

ui <- fluidPage(
  
  # App title ----
  titlePanel("GPA Calculator"),
  sidebarLayout(
    sidebarPanel(width = 3,
                 fileInput(inputId = "img_file", 
                           label = "Upload your display photo",
                           accept = c('image/png', 'image/jpeg', 'image/jpg')),
                 checkboxInput("showpic", "Show Photo", TRUE),
                 selectInput(inputId = "gradetype",
                             label = "Select Grade to Compute",
                             choices = c("Current GPA", "Current and Future GPA"),
                             selected = "Current GPA"),
                 # Current GPA
                 conditionalPanel(condition = "input.gradetype == 'Current GPA'",
                                  numericInput(inputId = "s10",
                                               label = "Last CGPA",
                                               min = 0,
                                               max = 4,
                                               value = 4),
                                  numericInput(inputId = "c10",
                                               label = "Credit:",
                                               min = 1,
                                               max = 70,
                                               value = 0,
                                               step = 1),
                                  sliderInput(inputId = "s11",
                                              label = "Subject 1",
                                              min = 0,
                                              max = 4,
                                              value = 4,
                                              step = 0.5),
                                  numericInput(inputId = "c11",
                                               label = "Credit:",
                                               min = 1,
                                               max = 20,
                                               value = 1,
                                               step = 1),
                                  sliderInput(inputId = "s12",
                                              label = "Subject 2",
                                              min = 0,
                                              max = 4,
                                              value = 4,
                                              step = 0.5),
                                  numericInput(inputId = "c12",
                                               label = "Credit:",
                                               min = 1,
                                               max = 20,
                                               value = 1,
                                               step = 1),
                                  sliderInput(inputId = "s13",
                                              label = "Subject 3",
                                              min = 0,
                                              max = 4,
                                              value = 4,
                                              step = 0.5),
                                  numericInput(inputId = "c13",
                                               label = "Credit:",
                                               min = 1,
                                               max = 20,
                                               value = 1,
                                               step = 1),
                                  radioButtons("gpatype", "GPA Type",
                                               choices = c(GPA = 1,
                                                           All = 2),
                                               selected = ","),
                 ),
                 # Future
                 conditionalPanel(condition = "input.gradetype == 'Current and Future GPA'",
                                  numericInput(inputId = "s20",
                                               label = "Last CGPA",
                                               min = 0,
                                               max = 4,
                                               value = 4),
                                  numericInput(inputId = "c20",
                                               label = "Credit:",
                                               min = 1,
                                               max = 70,
                                               value = 0,
                                               step = 1),
                                  sliderInput(inputId = "s21",
                                              label = "Subject 1",
                                              min = 0,
                                              max = 4,
                                              value = 4,
                                              step = 0.5),
                                  numericInput(inputId = "c21",
                                               label = "Credit:",
                                               min = 1,
                                               max = 20,
                                               value = 1,
                                               step = 1),
                                  sliderInput(inputId = "s22",
                                              label = "Subject 2",
                                              min = 0,
                                              max = 4,
                                              value = 4,
                                              step = 0.5),
                                  numericInput(inputId = "c22",
                                               label = "Credit:",
                                               min = 1,
                                               max = 20,
                                               value = 1,
                                               step = 1),
                                  sliderInput(inputId = "s23",
                                              label = "Subject 3",
                                              min = 0,
                                              max = 4,
                                              value = 4,
                                              step = 0.5),
                                  numericInput(inputId = "c23",
                                               label = "Credit:",
                                               min = 1,
                                               max = 20,
                                               value = 1,
                                               step = 1),
                                  sliderInput(inputId = "fs2",
                                              label = "Future GPA",
                                              min = 0,
                                              max = 4,
                                              value = c(2,4),
                                              step = 0.5),
                                  numericInput(inputId = "fc2",
                                               label = "Credit:",
                                               min = 1,
                                               max = 30,
                                               value = 1,
                                               step = 1),
                                  radioButtons("gpatype", "GPA Type",
                                               choices = c(GPA = 1,
                                                           All = 2),
                                               selected = ",")
                                  
                 ),
                 
                 actionButton(inputId = "pushstart", label = "Calculate")
                 
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      imageOutput("img_display", width = "200px", height = "200px"),
      
      tableOutput("mytable"),
      
      plotOutput("barPlot", width = "600px", height = "300px")
      
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  observeEvent(input$pushstart, {
  
  
  if (input$showpic){
    output$img_display <- renderImage({
      req(input$img_file)   # wait until file is uploaded
      
      list(src = input$img_file$datapath,
           contentType = input$img_file$type,
           width = "100%")  # scale to fit the mainPanel
    }, deleteFile = FALSE)  # keep the uploaded file available
  }
  

  

    
    if (input$gradetype == 'Current GPA'){
      gpa = (input$s11*input$c11 + input$s12*input$c12 + input$s13*input$c13)/(input$c11 + input$c12 + input$c13)
      cgpa = (input$s10*input$c10 + input$s11*input$c11 + input$s12*input$c12 + input$s13*input$c13)/(input$c10 + input$c11 + input$c12 + input$c13)
      if (input$gpatype == 1){
        output$mytable <- renderTable({
          # Make a 1x2 data frame
          data.frame(
            Type = c("GPA"),
            Grade = c(gpa)
          )
        })
      } else{
        df = data.frame(
          Type = c("GPA", "CGPA"),
          Grade = c(gpa, cgpa)
        )
        output$mytable <- renderTable({
          df
        })
      }
    } else if (input$gradetype == 'Current and Future GPA'){
      gpa = (input$s21*input$c21 + input$s22*input$c22 + input$s23*input$c23)/(input$c21 + input$c22 + input$c23)
      cgpa = (input$s20*input$c20 + input$s21*input$c21 + input$s22*input$c22 + input$s23*input$c23)/(input$c20 + input$c21 + input$c22 + input$c23)
      mincgpa = (input$s20*input$c20 + input$s21*input$c21 + input$s22*input$c22 + input$s23*input$c23 + input$fs2[1]*input$fc2)/(input$c20 + input$c21 + input$c22 + input$c23 + input$fc2)
      maxcgpa = (input$s20*input$c20 + input$s21*input$c21 + input$s22*input$c22 + input$s23*input$c23 + input$fs2[2]*input$fc2)/(input$c20 + input$c21 + input$c22 + input$c23 + input$fc2)
      if (input$gpatype == 1){
        output$mytable <- renderTable({
          data.frame(
            Type = c("GPA"),
            Grade = c(gpa)
          )
        })
      } else{
        df = data.frame(
          Type = c("GPA", "CGPA", "Min ECGPA", "Max ECGPA"),
          Grade = c(gpa, cgpa, mincgpa, maxcgpa))
        output$mytable <- renderTable({
          df
        })
        output$barPlot <- renderPlot({
          barplot(df$Grade,
                  names.arg = df$Type,
                  col = "steelblue",
                  main = "Bar Chart",
                  xlab = "Type", ylab = "Grade")
        })
      
      }
      
    }
  
  
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)