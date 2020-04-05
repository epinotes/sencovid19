
library(lubridate)
library(scales)
library(viridis)
library(ggthemes)
library(plotly)
library(tidyverse)

covid19_cas <- readr::read_csv("data/sencovid19.csv")

sencovid_rec <- readr::read_csv("data/sencovid_rec.csv")

covid19_cas <- covid19_cas %>%
  replace_na(list(origine = "Inconnu"))

covid19_cas <- covid19_cas %>% 
  count(date, origine) %>% 
  rename(nombre = n)

last_date <- format(max(covid19_cas$date), "%d/%m/%Y")

min_dt <- min(covid19_cas$date)
max_dt <- max(covid19_cas$date)+1

covid19_daily <- covid19_cas %>% 
  group_by(date) %>% 
  summarise_at(vars(nombre), sum) %>% 
  ungroup()
  
sen_total <- sum(covid19_cas$nombre)

gg_sen_epi <- covid19_cas  %>% 
  ggplot(aes(date, nombre, fill = origine)) +
  viridis::scale_fill_viridis(option = "D", discrete = T)+
  geom_bar(stat = "identity", position = position_stack()) +
  ggtitle(glue::glue("Cas de COVID-19 Confirme au Senegal(total = {sen_total})"))+
  scale_y_continuous(breaks = breaks_pretty(16)) +
  scale_x_date(breaks = scales::breaks_pretty(10), 
               labels = date_format(format = "%d-%b", tz = "UTC"),
               expand = expansion(0, 0), 
               limits = c(min_dt, max_dt))+
  theme_tufte() +
  theme(axis.text = element_text(size = 12))

gg_sen_epi2 <- gg_sen_epi + 
  geom_text(data = covid19_daily, aes(date, nombre, label = nombre), 
            size = 4, nudge_y = 0.5, inherit.aes = FALSE)


sen_epic2 <- ggplotly(gg_sen_epi2)

# sen_epic <- ggplotly(gg_sen_epi)

# htmlwidgets::saveWidget(sen_epic , "sencov_epic.html")


# cumulative --------------------------------------------------------------

covid19_cum <- covid19_cas %>% 
  group_by(date) %>% 
  summarise_at(vars(nombre), sum) %>% 
  ungroup() %>% 
  mutate(cumule = cumsum(nombre)) %>% 
  mutate(slopes = diff(c(0, cumule))/diff(lag(c(0, cumule))))


p_cum <- covid19_cum %>% select(-nombre) %>% 
ggplot(aes(date, cumule)) + 
  geom_point(color = "#F1605DFF") +
  geom_line(color = "#F1605DFF")+
  scale_y_continuous(breaks = breaks_pretty(15)) +
  scale_x_date(breaks = scales::breaks_pretty(15), 
               labels = date_format(format = "%d-%b", tz = "UTC"),
               expand = expansion(0, 0), 
               limits = c(min_dt, max_dt))+
  theme_tufte() +
  theme(axis.text = element_text(size = 12), legend.position = "none")

cum_epi <- ggplotly(p_cum)
