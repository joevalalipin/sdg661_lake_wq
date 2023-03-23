library(dplyr)
library(tidyr)
library(sf)
library(leaflet)
library(readr)

national_boundaries <- st_read("data/World_Countries_(Generalized)/World_Countries__Generalized_.shp") 

trophic_year_percent <- read_csv("data/gaul_0_trophic_year_percentage_ts_new.csv") %>% 
  mutate(parameter = "Trophic state")

turbidity_year_percent <- read_csv("data/gaul_0_turbidity_year_percentage_ts_new.csv") %>% 
  mutate(parameter = "Turbidity")

year_percent <- bind_rows(trophic_year_percent, turbidity_year_percent) %>% 
  mutate(ADM0_NAME = case_when(ADM0_NAME == "Bolivia (Plurinational State of)" ~ "Bolivia",
                               ADM0_NAME == "Czechia" ~ "Czech Republic",
                               ADM0_NAME == "C�te d'Ivoire" ~ "Côte d'Ivoire",
                               ADM0_NAME == "Democratic People's Republic of Korea" ~ "South Korea",
                               ADM0_NAME == "Democratic Republic of the Congo" ~ "Congo DRC",
                               ADM0_NAME == "Iran (Islamic Republic of)" ~ "Iran",
                               ADM0_NAME == "Jammu and Kashmir" ~ "Bolivia",
                               ADM0_NAME == "Lao People's Democratic Republic" ~ "Laos",
                               ADM0_NAME == "Syrian Arab Republic" ~ "Syria",
                               ADM0_NAME == "Turkey" ~ "Turkiye",
                               ADM0_NAME == "United Kingdom of Great Britain & Northern Ireland" ~ "United Kingdom",
                               ADM0_NAME == "United Republic of Tanzania" ~ "Tanzania",
                               ADM0_NAME == "United States of America" ~ "United States",
                               ADM0_NAME == "Viet Nam" ~ "Vietnam",
                               TRUE ~ ADM0_NAME))

countries <- national_boundaries %>% 
  as_tibble() %>% 
  select(COUNTRY) %>% 
  mutate(id = COUNTRY) %>% 
  full_join(year_percent %>% 
              select(ADM0_NAME) %>% 
              distinct() %>% 
              mutate(id = ADM0_NAME),
            by = "id")

countries %>% filter(is.na(COUNTRY)) %>% pull(ADM0_NAME)

year_percent <- national_boundaries %>% 
  left_join(year_percent, by = c("COUNTRY" = "ADM0_NAME")) %>% 
  gather(key = "value_type", value = "value", 10:13)

data <- year_percent %>% 
  filter(period == 2017,
         value_type == "medium",
         parameter == "Trophic state")

pal <- colorNumeric(palette = "viridis",
                    domain = data$value)
leaflet(data) %>%
  addTiles() %>%
  addPolygons(color = ~pal(value),
              stroke = FALSE,
              highlight = highlightOptions(fillOpacity = 0.4),
              label = paste0(data$COUNTRY, "\n", data$parameter, ": ", data$value)) %>% 
  addLegend(pal = pal,
            position = "bottomleft",
            values = data$value)

save(year_percent,
     file = "year_percent.rdata")
