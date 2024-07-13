library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(ggExtra)

current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

exam_data_all_clean <- readRDS("data/exam_data_all_clean.rds")


CHOICES <- list(
  skolas = c(
    "Skolas nosaukums" = "",
    unique(exam_data_all_clean$skola)
  )
)




ui <- page(
  selectizeInput(
    inputId = "skola",
    label = "Izvēlieties skolu",
    choices = CHOICES$skolas,
    multiple = TRUE,
    options = list(plugins = "remove_button", closeAfterSelect = TRUE)
  ),
  layout_column_wrap(
    width = 1/2,
    height = 300,
    card(
      height = 625,
      full_screen = TRUE,
      card_header("Matemātika"),
      plotOutput("math")
    ),
    card(
      height = 625,
      full_screen = TRUE,
      card_header("Latviešu valoda"),
      plotOutput("lat_val")
    )
    
  ),
  layout_column_wrap(
    width = 1/2,
    height = 300,
    card(
      height = 625,
      full_screen = TRUE,
      card_header("Dabas"),
      plotOutput("nature")
    ),
    card(
      height = 625,
      full_screen = TRUE,
      card_header("Angļu valoda"),
      plotOutput("eng")
    )
    
  ),
  layout_column_wrap(
    width = 1/2,
    height = 300,
    card(
      height = 625,
      full_screen = TRUE,
      card_header("Sociālie un humanitārie priekšmeti"),
      plotOutput("human")
    ),
    card(
      height = 625,
      full_screen = TRUE,
      card_header("Citas svešvalodas"),
      plotOutput("other_lang")
    )

    
  )
  

)


server <- function(input, output) {
  

  selected_data_math <- reactive({
    req(input$skola)
    exam_data_all_clean |>
      filter(skola %in% input$skola) |>
      filter(eksamens_grupa == "Matemātika")
  })
  
  selected_data_lat_val <- reactive({
    req(input$skola)
    exam_data_all_clean |>
      filter(skola %in% input$skola) |>
      filter(eksamens_grupa == "Latviešu valoda")
  })
  
  selected_data_eng <- reactive({
    req(input$skola)
    exam_data_all_clean |>
      filter(skola %in% input$skola) |>
      filter(eksamens_grupa == "Angļu valoda")
  })
  
  
  selected_data_other_lang <- reactive({
    req(input$skola)
    exam_data_all_clean |>
      filter(skola %in% input$skola) |>
      filter(eksamens_grupa %in% c("Franču valoda", "Vācu valoda", "Krievu valoda"))
  })
  
  
  selected_data_neature <- reactive({
    req(input$skola)
    exam_data_all_clean |>
      filter(skola %in% input$skola) |>
      filter(eksamens_grupa %in% c("Bioloģija",
                                   "Dizains un tehnoloģijas",
                                   "Fizika",
                                   "Ģeogrāfija",
                                   "Ķīmija",
                                   "Programmēšana"))
  })
  
  
  
  selected_data_human <- reactive({
    req(input$skola)
    exam_data_all_clean |>
      filter(skola %in% input$skola) |>
      filter(eksamens_grupa %in% c("Sociālās zinātnes",
                                   "Vēsture",
                                   "Kultūra un māksla"))
  })
  
  
  output$math <- renderPlot({
      ggplot(selected_data_math(), aes(x = procenti , y = eksamens_limenis, fill = skola)) +
      geom_boxplot(position = position_dodge(preserve = "single"), width = 0.5) +
      geom_jitter(position = position_jitterdodge(dodge.width = 0.5, jitter.height = 0.5), 
                  alpha = 0.25, size = 0.75) +
      coord_cartesian(xlim = c(1, 100))+
      scale_x_continuous(position = "top") +
      # scale_y_discrete(limits = rev, name = NULL)+
      theme_bw() +
      theme(
        axis.title.x = element_text(size = 16),
        axis.title.y = element_blank(),
        legend.position = "top",
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14)
        # legend.text = element_text(size = 12)
      )+
      scale_fill_manual(values = c("#ff7a86", "#f5b30a", "#9d91ec"))
      
  })
  
  output$lat_val <- renderPlot({
    ggplot(selected_data_lat_val(), aes(x = procenti , y = eksamens_limenis, fill = skola)) +
      geom_boxplot(position = position_dodge(preserve = "single"), width = 0.5) +
      geom_jitter(position = position_jitterdodge(dodge.width = 0.5, jitter.height = 0.5), 
                  alpha = 0.25, size = 0.75) +
      coord_cartesian(xlim = c(1, 100))+
      scale_x_continuous(position = "top") +
      # scale_y_discrete(limits = rev, name = NULL)+
      theme_bw() +
      theme(
        axis.title.x = element_text(size = 16),
        axis.title.y = element_blank(),
        legend.position = "top",
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14)
        # legend.text = element_text(size = 12)
      )+
      scale_fill_manual(values = c("#ff7a86", "#f5b30a", "#9d91ec"))
    
  })
  
  
  output$eng <- renderPlot({
    ggplot(selected_data_eng(), aes(x = procenti , y = eksamens_limenis, fill = skola)) +
      geom_boxplot(position = position_dodge(preserve = "single"), width = 0.5) +
      geom_jitter(position = position_jitterdodge(dodge.width = 0.5, jitter.height = 0.5), 
                  alpha = 0.25, size = 0.75) +
      coord_cartesian(xlim = c(1, 100))+
      scale_x_continuous(position = "top") +
      # scale_y_discrete(limits = rev, name = NULL)+
      theme_bw() +
      theme(
        axis.title.x = element_text(size = 16),
        axis.title.y = element_blank(),
        legend.position = "top",
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14)
        # legend.text = element_text(size = 12)
      )+
      scale_fill_manual(values = c("#ff7a86", "#f5b30a", "#9d91ec"))
    
  })
  
  
  output$other_lang <- renderPlot({
    ggplot(selected_data_other_lang(), aes(x = procenti , y = eksamens_name, fill = skola)) +
      geom_boxplot(position = position_dodge(preserve = "single"), width = 0.5) +
      geom_jitter(position = position_jitterdodge(dodge.width = 0.5, jitter.height = 0.5), 
                  alpha = 0.25, size = 0.75) +
      coord_cartesian(xlim = c(1, 100))+
      scale_x_continuous(position = "top") +
      # scale_y_discrete(limits = rev, name = NULL)+
      theme_bw() +
      theme(
        axis.title.x = element_text(size = 16),
        axis.title.y = element_blank(),
        legend.position = "top",
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14)
        # legend.text = element_text(size = 12)
      )+
      scale_fill_manual(values = c("#ff7a86", "#f5b30a", "#9d91ec"))
    
  })
  
  output$nature <- renderPlot({
    ggplot(selected_data_neature(), aes(x = procenti , y = eksamens_name, fill = skola)) +
      geom_boxplot(position = position_dodge(preserve = "single"), width = 0.5) +
      geom_jitter(position = position_jitterdodge(dodge.width = 0.5, jitter.height = 0.5), 
                  alpha = 0.25, size = 0.75) +
      coord_cartesian(xlim = c(1, 100))+
      scale_x_continuous(position = "top") +
      # scale_y_discrete(limits = rev, name = NULL)+
      theme_bw() +
      theme(
        axis.title.x = element_text(size = 16),
        axis.title.y = element_blank(),
        legend.position = "top",
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14)
        # legend.text = element_text(size = 12)
      )+
      scale_fill_manual(values = c("#ff7a86", "#f5b30a", "#9d91ec"))
    
  })
  
  
  
  output$human <- renderPlot({
    ggplot(selected_data_human(), aes(x = procenti , y = eksamens_name, fill = skola)) +
      geom_boxplot(position = position_dodge(preserve = "single"), width = 0.5) +
      geom_jitter(position = position_jitterdodge(dodge.width = 0.5, jitter.height = 0.5), 
                  alpha = 0.25, size = 0.75) +
      coord_cartesian(xlim = c(1, 100))+
      scale_x_continuous(position = "top") +
      # scale_y_discrete(limits = rev, name = NULL)+
      theme_bw() +
      theme(
        axis.title.x = element_text(size = 16),
        axis.title.y = element_blank(),
        legend.position = "top",
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14)
        # legend.text = element_text(size = 12)
      )+
      scale_fill_manual(values = c("#ff7a86", "#f5b30a", "#9d91ec"))
    
  })
  
  
}


shinyApp(ui, server)