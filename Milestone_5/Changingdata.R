library(tidyverse)
library(readr)
library(shiny)
library(shinythemes)
library(janitor)
library(readr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(PPBDS.data)

source("reading_in.R")


second_clean <- first_clean %>%
  select(country_name, country_code, indicator_name, x2015:x2018) %>%
  drop_na() %>%
  filter(indicator_name %in% c("Total natural resources rents (% of GDP)","Tax revenue (% of GDP)")) %>%
  mutate(average = (x2015 + x2016 + x2017 +x2018)/4) %>%
  filter(average > 0) %>%
  pivot_wider(names_from = indicator_name, values_from = average, id_cols = c("country_name", "country_code")) %>%
  drop_na() %>%
  clean_names()

first_graph <- second_clean %>% 
  ggplot(aes(x = total_natural_resources_rents_percent_of_gdp, y = tax_revenue_percent_of_gdp)) +
  geom_point(color = "red") +
  geom_smooth(method = lm,
              se = FALSE,
              color = "dodgerblue") +
  labs(x = "Natural Resources Rent as % of GDP",
       y = "Tax Revenue as % of GDP",
       title = "Relationship Between Tax Revenue and Natural Resources Rent as % of GDP",
       subtitle  = "Does Depending on Natural Resources Mean Taxing Less?",
       caption = "Source: World Bank Development Inidicators")

library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")


countrynames_worldmap <- world %>%
  select(name, geometry) %>%
  mutate(name = as.factor(name)) %>%
  rename(country_name = name)
 

world_map <- countrynames_worldmap %>%
  ggplot() +
  geom_sf()

view(first_clean)

new <- countrynames_worldmap %>%
left_join(first_clean, by = "country_name") %>%
select(geometry, country_name, indicator_name, x2015:x2018) %>%
mutate(average = mean(x2015, x2018))
glimpse(new)



