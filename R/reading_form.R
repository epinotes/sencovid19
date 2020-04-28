library(googlesheets4)
library(lubridate)
library(scales)
library(viridis)
library(ggthemes)
library(plotly)
library(tidyverse)


sencovid_cumule <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1C7h0IiZixUgUpwJrj9E3DPGKLMwOiQio3VVBMW8EYcc/edit?usp=sharing")

sencovid_cumule <- sencovid_cumule %>% 
  mutate_all(unlist) %>% 
  as.data.frame()

readr::write_csv(sencovid_cumule, "data/sencovid_cumule.csv")

# https://rmarkdown.rstudio.com/flexdashboard/using.html

# sencovid_track <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1syQTnmjdy8lrFrfPV0v2wb_dWMkM4DTNaTY4u3W_sqM/edit?usp=sharing")
# 
# # covid_track <- readr::read_csv("data_raw/Senegal COVID-19 Epi .csv")
# 
# sencovid_rec <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1GybI_-Hf44b40kSwNYAtwTp7q3IOyAct4MNPXcXUEJ8/edit?usp=sharing")
# 
# sencovid_rec <- sencovid_rec %>% 
#   mutate(nombre_gueris = as.numeric(unlist(nombre_gueris)),
#          nombre_deces = as.numeric(unlist(nombre_deces))) %>% 
#   filter(communique_date == max(communique_date)) %>% 
#   as.data.frame()
# 
# sencovid_rec
# 
# dim(sencovid_track)
# 
# sencovid_track <- sencovid_track %>% 
#   mutate(Timestamp = ymd_hms(Timestamp),
#          date = as.Date(date),
#          country_origin = str_to_title(country_origin),
#          country_origin = gsub("\\W+|\\d+", "", country_origin, perl = T)) 
# 
# unique(sencovid_track$country_origin)
# 
# sencovid_track <- sencovid_track %>% 
#   group_by(id) %>% 
#   filter(Timestamp == max(Timestamp)) %>% 
#   ungroup() %>% 
#   rename(ville = city,
#          sexe = sex,
#          origine = country_origin) %>%
#   replace_na(list(origine = "Inconnu"))
# 
# sencovid19 <- sencovid_track %>% 
#   select(2:8)
# 
# readr::write_csv(sencovid19, "data/sencovid19.csv")
# 
# readr::write_csv(sencovid_rec, "data/sencovid_rec.csv")
# 


