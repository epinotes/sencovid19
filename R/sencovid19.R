
library(lubridate)
library(scales)
library(viridis)
library(ggthemes)
library(plotly)
library(tidyverse)

covid19_cas <- readr::read_csv("data/sencovid19.csv")

covid19_cas <- covid19_cas %>%
  replace_na(list(origine = "Inconnu"))

covid19_cas <- covid19_cas %>% 
  count(date, origine) %>% 
  rename(nombre = n)

min_dt <- min(covid19_cas$date)
max_dt <- max(covid19_cas$date)


sen_total <- sum(covid19_cas$nombre)

gg_sen_epi <- covid19_cas  %>% 
  ggplot(aes(date, nombre, fill = origine)) +
  viridis::scale_fill_viridis(option = "D", discrete = T)+
  geom_bar(stat = "identity", position = position_stack()) +
  ggtitle(glue::glue("Cas de COVID-19 Confirme au Senegal(total = {sen_total})"))+
  scale_y_continuous(breaks = breaks_pretty(15)) +
  # scale_y_continuous(breaks = seq_along(0:wa_total_c)) +
  scale_x_date(breaks = scales::breaks_pretty(8), expand = expansion(0, 0), 
               limits = c(min_dt, max_dt))+
  theme_tufte() +
  theme(axis.text = element_text(size = 12)) 

p <- ggplotly(gg_sen_epi)

htmlwidgets::saveWidget(p, "sencov_epic.html")

