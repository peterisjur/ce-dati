library(shiny)

shinyUI(fluidPage(
  titlePanel("Exam Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "selected_schools",
        label = "Select Schools",
        choices = unique(exam_data_all_clean$school),
        selected = unique(exam_data_all_clean$school)[1],
        multiple = TRUE,
        selectize = TRUE
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "boxplot")
    )
  )
))
