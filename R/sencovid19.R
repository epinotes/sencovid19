
library(lubridate)
library(scales)
library(viridis)
library(ggthemes)
library(plotly)
library(tidyverse)

covid19_cas <- readr::read_csv("data/sencovid19.csv")

# sencovid_rec <- readr::read_csv("data/sencovid_rec.csv")

sencovid_cumule <- readr::read_csv("data/sencovid_cumule.csv")

covid19_cas <- covid19_cas %>%
  replace_na(list(origine = "Inconnu"))

covid19_cas <- covid19_cas %>% 
  mutate(origine = if_else(origine %in% c("Inconnue", "Senegal"), "Communautaire", "Importe"))

covid19_cas <- covid19_cas %>% 
  count(date, origine) %>% 
  rename(nombre = n)

### new method

covid19_cas_arch <- covid19_cas %>% 
  group_by(origine) %>% 
  summarise_at(vars(nombre), sum) %>% 
  pivot_wider(names_from = origine, values_from = nombre)

total_arch <- sum(covid19_cas$nombre)

date_lx = max(covid19_cas$date)

covid19_cumule_24 <- tibble(date = date_lx, nombre_importes = 0, total_confirmes = total_arch)

sencovid_cum_x <- sencovid_cumule %>%
  mutate_at(vars(matches("nombre|total")), as.numeric) %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(date) %>% 
  filter(Timestamp == max(Timestamp)) %>% 
  select(date, nombre_importes, total_confirmes)

sencovid_cum <- covid19_cumule_24 %>% 
  bind_rows(sencovid_cum_x) %>% 
  mutate(nombre_confirmes = c(0, diff(total_confirmes))) %>% 
  slice(-1)

sencovid_cum <- sencovid_cum %>% 
  mutate(Communautaire = nombre_confirmes - nombre_importes,
         Importe = nombre_importes) %>%
  select(date, Communautaire, Importe) %>% 
  pivot_longer(cols = -date, names_to = "origine", values_to = "nombre")

covid19_cas2 <- covid19_cas %>% 
  bind_rows(sencovid_cum)

###

last_date <- format(max(covid19_cas2$date), "%d/%m/%Y")

min_dt <- min(covid19_cas2$date)
max_dt <- max(covid19_cas2$date)+1

covid19_daily <- covid19_cas2 %>% 
  group_by(date) %>% 
  summarise_at(vars(nombre), sum) %>% 
  ungroup()

  
sen_total <- sum(covid19_cas2$nombre)

date_lx = max(covid19_cas2$date)

gg_sen_epi <- covid19_cas2  %>% 
  ggplot(aes(date, nombre, fill = origine))+
  viridis::scale_fill_viridis(option = "D", discrete = T, direction = -1)+
  geom_bar(stat = "identity", position = position_stack()) +
  ggtitle(glue::glue("Cas de COVID-19 Confirme au Senegal(total = {sen_total}) le {format.Date(max_dt - 1, format = '%d %B %Y')}"))+
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

# ggsave("graphique/sen_epicurve.png", width = 9, height = 7)

sen_epic2 <- ggplotly(gg_sen_epi2)

 

# sen_epic <- ggplotly(gg_sen_epi)

# htmlwidgets::saveWidget(sen_epic , "sencov_epic.html")


# cumulative --------------------------------------------------------------

covid19_cum <- covid19_cas2 %>% 
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

# table

sen_dt <- covid19_daily %>% 
  DT::datatable(caption = "Nombre de Cas Confirmes par Date",
                
                rownames = FALSE,
                
                extensions = 'Buttons',
                
                filter = list(position = 'top', clear = FALSE),
                
                options = list(
                  
                  pageLength = 10, dom = 'Bfrtip',
                  
                  buttons = c('copy', 'csv'),
                  
                  columnDefs = list(list(className = 'dt-left', targets = 1))
                  
                ))
