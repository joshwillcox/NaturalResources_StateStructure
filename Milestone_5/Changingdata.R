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


first_graph <- first_clean %>%
  select(country_name, country_code, indicator_name, x2015:x2018) %>%
  drop_na() %>%
  filter(indicator_name %in% c("Total natural resources rents (% of GDP)","Tax revenue (% of GDP)")) %>%
  mutate(average = (x2015 + x2016 + x2017 +x2018)/4) %>%
  filter(average > 0) %>%
  pivot_wider(names_from = indicator_name, values_from = average, id_cols = c("country_name", "country_code")) %>%
  drop_na() %>%
  clean_names() %>% 
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



fixed_names1 <- first_clean %>%
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
  mutate(country_name = str_replace(country_name, pattern = "Antigua and Barbuda", replacement = "Antigua and Barb.")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Bahamas, The", replacement = "Bahamas")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Bosnia and Herzegovina", replacement = "Bosnia and Herz.")) %>%
  mutate(country_name = str_replace(country_name, pattern = "British Virgin Islands", replacement = "British Virgin Is.")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Brunei Darussalam", replacement = "Brunei")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Cabo Verde", replacement = "Cape Verde")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Cayman Islands", replacement = "Cayman Is.")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Czech Republic", replacement = "Czech Rep.")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Dominican Republic", replacement = "Dominican Rep.")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Equatorial Guinea", replacement = "Eq. Guinea")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Eswatini", replacement = "Swaziland")) %>%
  mutate(country_name = str_replace(country_name, pattern = "French Polynesia", replacement = "Fr. Polynesia")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Gambia, The", replacement = "Gambia")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Hong Kong SAR, China", replacement = "Hong Kong")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Korea, Dem. People’s Rep.", replacement = "Dem. Rep. Korea")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Korea, Rep.", replacement = "Korea")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Kyrgyz Republic", replacement = "Kyrgyzstan")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Macao SAR, China", replacement = "Macao")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Marshall Islands", replacement = "Marshall Is.")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Micronesia, Fed. Sts.", replacement = "Micronesia")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Sao Tome and Principe", replacement = "São Tomé and Principe")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Slovak Republic", replacement = "Slovakia")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Solomon Islands", replacement = "Solomon Is.")) %>%
  mutate(country_name = str_replace(country_name, pattern = "St. Lucia", replacement = "St. Vin. and Gren.")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Turks and Caicos Islands", replacement = "Turks and Caicos Is.")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Virgin Islands (U.S.)", replacement = "U.S. Virgin Is.")) %>%
  mutate(country_name = str_replace(country_name, pattern = "West Bank and Gaza", replacement = "Palestine")) %>%
  mutate(country_name = str_replace(country_name, pattern = "St. Lucia", replacement = "Saint Lucia")) %>%
  select(! x2017:x66)

fixed_names2 <- second_clean %>%
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
  mutate(country_name = str_replace(country_name, pattern = "Antigua and Barbuda", replacement = "Antigua and Barb.")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Bahamas, The", replacement = "Bahamas")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Bosnia and Herzegovina", replacement = "Bosnia and Herz.")) %>%
  mutate(country_name = str_replace(country_name, pattern = "British Virgin Islands", replacement = "British Virgin Is.")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Brunei Darussalam", replacement = "Brunei")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Cabo Verde", replacement = "Cape Verde")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Cayman Islands", replacement = "Cayman Is.")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Czech Republic", replacement = "Czech Rep.")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Dominican Republic", replacement = "Dominican Rep.")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Equatorial Guinea", replacement = "Eq. Guinea")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Eswatini", replacement = "Swaziland")) %>%
  mutate(country_name = str_replace(country_name, pattern = "French Polynesia", replacement = "Fr. Polynesia")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Gambia, The", replacement = "Gambia")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Hong Kong SAR, China", replacement = "Hong Kong")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Korea, Dem. People’s Rep.", replacement = "Dem. Rep. Korea")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Korea, Rep.", replacement = "Korea")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Kyrgyz Republic", replacement = "Kyrgyzstan")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Macao SAR, China", replacement = "Macao")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Marshall Islands", replacement = "Marshall Is.")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Micronesia, Fed. Sts.", replacement = "Micronesia")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Sao Tome and Principe", replacement = "São Tomé and Principe")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Slovak Republic", replacement = "Slovakia")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Solomon Islands", replacement = "Solomon Is.")) %>%
  mutate(country_name = str_replace(country_name, pattern = "St. Lucia", replacement = "St. Vin. and Gren.")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Turks and Caicos Islands", replacement = "Turks and Caicos Is.")) %>%
  mutate(country_name = str_replace(country_name, pattern = "Virgin Islands (U.S.)", replacement = "U.S. Virgin Is.")) %>%
  mutate(country_name = str_replace(country_name, pattern = "West Bank and Gaza", replacement = "Palestine")) %>%
  mutate(country_name = str_replace(country_name, pattern = "St. Lucia", replacement = "Saint Lucia"))

fixed_names <- fixed_names1 %>%
  rbind(fixed_names2)








  
  
  





