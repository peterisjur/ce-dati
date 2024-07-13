library(tidyverse)
library(ggplot2)
library(readr)

exam_data_all_clean <- read_rds("shiny_app/data/exam_data_all_clean.rds")


select_skola <- c("Rīgas 1. Tālmācības vidusskola", "Rīgas 84. vidusskola", "Rīgas Angļu ģimnāzija")


specific_school <- exam_data_all_clean %>%
  filter(skola %in% select_skola)%>%
  mutate(skola = factor(skola, levels = rev(select_skola)))



str(specific_school)
head(specific_school)



specific_school%>%
  ggplot(aes(x = procenti , y = eksamens_name, fill = skola)) +
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




tabula_ar_visu_skolu_datiem = DT::renderDataTable({
  tabula_ar_visu_skolu_datiem <- specific_school %>%
    select(skola, eksamens_grupa, nr.) %>%
    filter(eksamens == "Latviešu valoda") %>%
    unique() %>%
    group_by(skola) %>%
    summarise("skola" = skola, "skolēnu skaits" = n()) %>%
    unique()%>%
    datatable(rownames = FALSE,
              filter = "top",
              extensions = "Responsive",
              options = list(
                pageLength = 10,
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.20/i18n/Latvian.json') 
              )
    )})
  
tabula_ar_visu_skolu_datiem
