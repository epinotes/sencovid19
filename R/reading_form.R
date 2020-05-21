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
  mutate_at(vars(matches("nombre|total")), as.numeric) %>% 
  mutate(date = as.Date(date)) %>% 
  as.data.frame()

readr::write_csv(sencovid_cumule, "data/sencovid_cumule.csv")

