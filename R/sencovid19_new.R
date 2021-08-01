# remotes::install_github("joachim-gassen/tidycovid19")

library(tidycovid19)
library(tidyverse)
library(scales)
library(zoo)
library(plotly)

# https://rpubs.com/epinotes/senegalcovid19

updates_x <- tidycovid19::download_merged_data(cached = TRUE)
covid19_sen <- updates_x %>% 
  filter(country == "Senegal",
         !is.na(confirmed),
         confirmed > 0)

find_daily <- function(x){
  x2 = x-lag(x)
  x2[1] = x[1]
  return(x2)
}

covid19_sen <- covid19_sen %>%
  mutate(across(c(confirmed, deaths, recovered),
                .fns = find_daily,
                .names = "{col}_daily")) %>% 
  select(iso3c, country, date, confirmed, deaths, recovered, 
         confirmed_daily, deaths_daily, recovered_daily, everything()) 


last_date <- format(max(covid19_sen$date),"%B %d %Y")

min_dt <- min(covid19_sen$date)
max_dt <- max(covid19_sen$date)+1
# 
# covid19_daily <- covid19_cas2 %>% 
#   group_by(date) %>% 
#   summarise_at(vars(nombre), sum) %>% 
#   ungroup()


total_confirmes <- last(covid19_sen$confirmed)
total_gueris <- last(covid19_sen$recovered)
total_decedes <- last(covid19_sen$deaths)

date_lx = max(covid19_sen$date)

# covid19_cas2 <- covid19_cas2 %>% 
#   mutate(origine = gsub("Communautaire", "Endogene", origine))

gg_sen_epi <- covid19_sen  %>% 
  rename(nombre = confirmed_daily) %>% 
  mutate(weekly_average = 
           rollmean(nombre, 7, na.pad=TRUE, align="right")) %>%
  ggplot(aes(x = date)) +
  geom_bar(aes(y = nombre), stat = "identity", fill = "lightgreen") +
  geom_line(aes(y = weekly_average), color ="red") +
  ggtitle(glue::glue("Cas de COVID-19 Confirmes au Senegal(total = {total_confirmes}) le {format.Date(max_dt - 1, format = '%d %B %Y')}"))+
  scale_y_continuous(breaks = breaks_pretty(16)) +
  scale_x_date(breaks = scales::breaks_pretty(10), 
               labels = date_format(format = "%d-%b", tz = "UTC"),
               expand = expansion(0, 0), 
               limits = c(min_dt, max_dt)) +
  theme(axis.text = element_text(size = 11))+
  theme_minimal()

# gg_sen_epi2 <- gg_sen_epi + 
#   geom_text(data = covid19_daily, aes(date, nombre, label = nombre), 
#             size = 4, nudge_y = 0.5, inherit.aes = FALSE)

# ggsave("graphique/sen_epicurve.png", width = 9, height = 7)

sen_epic2 <- ggplotly(gg_sen_epi)



# sen_epic <- ggplotly(gg_sen_epi)

# htmlwidgets::saveWidget(sen_epic , "sencov_epic.html")


# cumulative --------------------------------------------------------------

# covid19_cum <- covid19_cas2 %>% 
#   group_by(date) %>% 
#   summarise_at(vars(nombre), sum) %>% 
#   ungroup() %>% 
#   mutate(cumule = cumsum(nombre)) %>% 
#   mutate(slopes = diff(c(0, cumule))/diff(lag(c(0, cumule))))


p_cum <- covid19_sen  %>% 
  rename(cumule = confirmed) %>% 
  ggplot(aes(date, cumule)) + 
  geom_point(color = "#F1605DFF") +
  geom_line(color = "#F1605DFF")+
  scale_y_continuous(breaks = breaks_pretty(15)) +
  scale_x_date(breaks = scales::breaks_pretty(15), 
               labels = date_format(format = "%d-%b", tz = "UTC"),
               expand = expansion(0, 0), 
               limits = c(min_dt, max_dt))+
  theme_minimal() +
  theme(axis.text = element_text(size = 12), legend.position = "none")

cum_epi <- ggplotly(p_cum)

# table

sen_dt <- covid19_sen %>%
  select(date, 
         confirmes = confirmed, 
         decedes = deaths, 
         gueris = recovered, 
         confirmes_quotidiens = confirmed_daily, 
         decedes_quotidiens = deaths_daily, 
         gueris_quotidiens = recovered_daily) 

sen_dt %>% 
  write_csv("data/sencovid19.csv")
