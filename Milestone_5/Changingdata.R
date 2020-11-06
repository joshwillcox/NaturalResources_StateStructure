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
  rename(country_name = name)
 

world_map <- countrynames_worldmap %>%
  ggplot() +
  geom_sf()

view(first_clean)



Country_Indicator <- first_clean %>%
  filter(indicator_name == "Oil rents (% of GDP)") %>%
  mutate(country_name = str_replace(country_name, pattern = "Russian Federation", replacement = "Russia")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Yemen, Rep.", replacement = "Yemen")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Venezuela, RB", replacement = "Venezuela")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Iran, Islamic Rep.", replacement = "Iran")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Egypt, Arab Rep.", replacement = "Egypt")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Syrian Arab Republic", replacement = "Syria")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Congo, Dem. Rep.", replacement = "Dem. Rep. Congo")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Congo, Rep.", replacement = "Congo")) %>%
  mutate(country_name = str_replace(country_name, pattern = "South Sudan", replacement = "S. Sudan")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Central African Republic", replacement = "Central African Rep.")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Cote d'Ivoire", replacement = "Côte d'Ivoire")) %>%
   select(country_name, indicator_name, x2010:x2018) %>%
  pivot_longer(cols = x2010:x2018, names_to = "year", values_to = "Percentage") %>%
  group_by(country_name) %>%
  summarise(average = mean(Percentage, na.rm = TRUE))

new <- countrynames_worldmap %>%
  left_join(Country_Indicator, by = "country_name") %>%
  ggplot(aes(fill = average)) +
  geom_sf()

class(new)

glimpse(Country_Indicator)

glimpse(countrynames_worldmap)

  




