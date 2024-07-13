library(shiny)
library(tidyverse)
library(ggplot2)

eksamens_grupa_seciba <- c(
  "Latviešu valoda",
  "Matemātika",
  "Angļu valoda",
  "Bioloģija",
  "Fizika",
  "Ķīmija",
  "Vēsture",
  "Vācu valoda",
  "Franču valoda",
  "Krievu valoda"
)

eksamens_limenis_seciba <- c(
  "augstākais līmenis",
  "optimālais līmenis",
  "vispārīgais līmenis",
  "4. kurss"
)

exam_data_all_clean <- read_csv("shiny_app/data/exam_data.csv") %>%
  mutate(
    eksamens_grupa = fct_relevel(eksamens_grupa, eksamens_grupa_seciba),
    eksamens_limenis = fct_relevel(eksamens_limenis, eksamens_limenis_seciba)
  )

shinyServer(function(input, output) {
  
  
  

})

