library(tidyverse)
library(openxlsx)


ce_vsk_data_2022_23_raw<-read.xlsx("data/raw/ce_datukopa_vsk_2022_2023.xlsx")
ce_exam_rename_raw<-read.xlsx("data/raw/exam_rename.xlsx")


ce_vsk_data_2022_23 <- ce_vsk_data_2022_23_raw%>%
  mutate(gads="2022/2023")%>%
  mutate(skola = ifelse(is.na(`Izglītības.iestādes,.kuras.struktūrvienība.ir.iestāda.nosaukums`),
                              `Izsglītības.iestādes.nosaukums`,
                              `Izglītības.iestādes,.kuras.struktūrvienība.ir.iestāda.nosaukums`))%>%
  mutate(skolas_vieniba = ifelse(is.na(`Izglītības.iestādes,.kuras.struktūrvienība.ir.iestāda.nosaukums`),
                                   NA,
                                   `Izsglītības.iestādes.nosaukums`))%>%
  left_join(ce_exam_rename_raw, by="Pārbaudījums")%>%
  rename(eksamens="Pārbaudījums",
         procenti="Kopvērtējums,.%",
         eksamens_grupa="Grupa",
         eksamens_limenis="Līmenis")%>%
  mutate(procenti=as.numeric(procenti))%>%
  select(skola, skolas_vieniba, eksamens, eksamens_grupa, eksamens_limenis, procenti, gads)


write_csv(ce_vsk_data_2022_23, "data/cleaned/ce_vsk_data_2022_23.csv")




eksamens_grupa_seciba <- c(
  "Latviešu valoda",
  "Matemātika",
  "Angļu valoda",
  "Bioloģija",
  "Fizika",
  "Ķīmija",
  "Vēsture",
  "Dizains un tehnoloģijas",
  "Ģeogrāfija",
  "Kultūra un māksla",
  "Programmēšana",
  "Sociālās zinātnes",
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

eksamens_name_levels <- expand.grid(
  eksamens_grupa = eksamens_grupa_seciba,
  eksamens_limenis = eksamens_limenis_seciba
) %>%
  mutate(eksamens_name = paste0(eksamens_grupa, ', ', eksamens_limenis)) %>%
  arrange(match(eksamens_grupa, eksamens_grupa_seciba), match(eksamens_limenis, eksamens_limenis_seciba))%>%
  pull(eksamens_name)




exam_data_all_clean <- ce_vsk_data_2022_23 %>%
  mutate(
    eksamens_grupa = fct_relevel(eksamens_grupa, eksamens_grupa_seciba),
    eksamens_limenis = fct_relevel(eksamens_limenis, eksamens_limenis_seciba),
    eksamens_name = factor(paste0(eksamens_grupa, ', ', eksamens_limenis), levels = eksamens_name_levels)
  )



write_rds(exam_data_all_clean, "shiny_app/data/exam_data_all_clean.rds")




