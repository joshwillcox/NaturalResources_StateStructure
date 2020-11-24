
## Loading all my packages

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
library(rstanarm)
library(rnaturalearth)
library(rnaturalearthdata)
library(gtsummary)
library(broom.mixed)
library(gt)


## Reading in the Data in order to create my graphs and tables and models.

source("reading_in.R")

## Data Changing

fixed_names1 <- first_clean %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Russian Federation",
                                    replacement = "Russia")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Yemen, Rep.",
                                    replacement = "Yemen")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Venezuela, RB",
                                    replacement = "Venezuela")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Iran, Islamic Rep.",
                                    replacement = "Iran")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Egypt, Arab Rep.",
                                    replacement = "Egypt")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Syrian Arab Republic",
                                    replacement = "Syria")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Congo, Dem. Rep.",
                                    replacement = "Dem. Rep. Congo")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Congo, Rep.",
                                    replacement = "Congo")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "South Sudan",
                                    replacement = "S. Sudan")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Central African Republic",
                                    replacement = "Central African Rep.")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Cote d'Ivoire",
                                    replacement = "Côte d'Ivoire")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Antigua and Barbuda",
                                    replacement = "Antigua and Barb.")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Bahamas, The",
                                    replacement = "Bahamas")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Bosnia and Herzegovina",
                                    replacement = "Bosnia and Herz.")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "British Virgin Islands", 
                                    replacement = "British Virgin Is.")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Brunei Darussalam",
                                    replacement = "Brunei")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Cabo Verde",
                                    replacement = "Cape Verde")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Cayman Islands",
                                    replacement = "Cayman Is.")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Czech Republic",
                                    replacement = "Czech Rep.")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Dominican Republic",
                                    replacement = "Dominican Rep.")) %>%
  mutate(country_name = str_replace(country_name, 
                                    pattern = "Equatorial Guinea",
                                    replacement = "Eq. Guinea")) %>%
  mutate(country_name = str_replace(country_name, 
                                    pattern = "Eswatini",
                                    replacement = "Swaziland")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "French Polynesia",
                                    replacement = "Fr. Polynesia")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Gambia, The", 
                                    replacement = "Gambia")) %>%
  mutate(country_name = str_replace(country_name, 
                                    pattern = "Hong Kong SAR, China",
                                    replacement = "Hong Kong")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Korea, Dem. People’s Rep.",
                                    replacement = "Dem. Rep. Korea")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Korea, Rep.", 
                                    replacement = "Korea")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Kyrgyz Republic",
                                    replacement = "Kyrgyzstan")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Macao SAR, China",
                                    replacement = "Macao")) %>%
  mutate(country_name = str_replace(country_name, 
                                    pattern = "Marshall Islands",
                                    replacement = "Marshall Is.")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Micronesia, Fed. Sts.",
                                    replacement = "Micronesia")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Sao Tome and Principe",
                                    replacement = "São Tomé and Principe")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Slovak Republic", 
                                    replacement = "Slovakia")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Solomon Islands", 
                                    replacement = "Solomon Is.")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "St. Lucia",
                                    replacement = "St. Vin. and Gren.")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Turks and Caicos Islands",
                                    replacement = "Turks and Caicos Is.")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Virgin Islands (U.S.)",
                                    replacement = "U.S. Virgin Is.")) %>%
  mutate(country_name = str_replace(country_name, 
                                    pattern = "West Bank and Gaza",
                                    replacement = "Palestine")) %>%
  mutate(country_name = str_replace(country_name, 
                                    pattern = "St. Lucia",
                                    replacement = "Saint Lucia")) %>%
  select(! x2017:x66)

fixed_names2 <- second_clean %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Russian Federation",
                                    replacement = "Russia")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Yemen, Rep.",
                                    replacement = "Yemen")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Venezuela, RB",
                                    replacement = "Venezuela")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Iran, Islamic Rep.",
                                    replacement = "Iran")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Egypt, Arab Rep.",
                                    replacement = "Egypt")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Syrian Arab Republic",
                                    replacement = "Syria")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Congo, Dem. Rep.",
                                    replacement = "Dem. Rep. Congo")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Congo, Rep.",
                                    replacement = "Congo")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "South Sudan",
                                    replacement = "S. Sudan")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Central African Republic",
                                    replacement = "Central African Rep.")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Cote d'Ivoire",
                                    replacement = "Côte d'Ivoire")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Antigua and Barbuda",
                                    replacement = "Antigua and Barb.")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Bahamas, The",
                                    replacement = "Bahamas")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Bosnia and Herzegovina",
                                    replacement = "Bosnia and Herz.")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "British Virgin Islands", 
                                    replacement = "British Virgin Is.")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Brunei Darussalam",
                                    replacement = "Brunei")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Cabo Verde",
                                    replacement = "Cape Verde")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Cayman Islands",
                                    replacement = "Cayman Is.")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Czech Republic",
                                    replacement = "Czech Rep.")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Dominican Republic",
                                    replacement = "Dominican Rep.")) %>%
  mutate(country_name = str_replace(country_name, 
                                    pattern = "Equatorial Guinea",
                                    replacement = "Eq. Guinea")) %>%
  mutate(country_name = str_replace(country_name, 
                                    pattern = "Eswatini",
                                    replacement = "Swaziland")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "French Polynesia",
                                    replacement = "Fr. Polynesia")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Gambia, The", 
                                    replacement = "Gambia")) %>%
  mutate(country_name = str_replace(country_name, 
                                    pattern = "Hong Kong SAR, China",
                                    replacement = "Hong Kong")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Korea, Dem. People’s Rep.",
                                    replacement = "Dem. Rep. Korea")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Korea, Rep.", 
                                    replacement = "Korea")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Kyrgyz Republic",
                                    replacement = "Kyrgyzstan")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Macao SAR, China",
                                    replacement = "Macao")) %>%
  mutate(country_name = str_replace(country_name, 
                                    pattern = "Marshall Islands",
                                    replacement = "Marshall Is.")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Micronesia, Fed. Sts.",
                                    replacement = "Micronesia")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Sao Tome and Principe",
                                    replacement = "São Tomé and Principe")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Slovak Republic", 
                                    replacement = "Slovakia")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Solomon Islands", 
                                    replacement = "Solomon Is.")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "St. Lucia",
                                    replacement = "St. Vin. and Gren.")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Turks and Caicos Islands",
                                    replacement = "Turks and Caicos Is.")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Virgin Islands (U.S.)",
                                    replacement = "U.S. Virgin Is.")) %>%
  mutate(country_name = str_replace(country_name, 
                                    pattern = "West Bank and Gaza",
                                    replacement = "Palestine")) %>%
  mutate(country_name = str_replace(country_name, 
                                    pattern = "St. Lucia",
                                    replacement = "Saint Lucia"))

# I had to create these fixed_names1 and 2 because the country names needed 
# to match the countrynames_worldmap data frame. There were lots of countries
# with "The" that needed removing or "Rep." that needed to be replaced with 
# "Republic" or "Is." with "Islands". There is no difference between fixed 1
# and fixed 2 other than the fact that they have different indicators and 
# I got them at different times.

fixed_names <- fixed_names1 %>%
  rbind(fixed_names2)

# I binded them together to create the full data set for the project.



world <- ne_countries(scale = "medium",
                      returnclass = "sf")


countrynames_worldmap <- world %>%
  select(name,
         geometry) %>%
  rename(country_name = name)

# This gives me a data set for all the world's countreis and their geometry.
# I then use it to create all the maps below.

regime_filter <- regime_type %>%
  select(country, year, regime_r) %>%
  filter(year %in% 2011) %>%
  rename(country_name = country) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Republic",
                                    replacement = "Rep.")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Slovak Rep.",
                                    replacement = "Slovakia")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Bosnia",
                                    replacement = "Bosnia and Herz.")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Equatorial Guinea",
                                    replacement = "Eq. Guinea")) %>%
  mutate(country_name = str_replace(country_name, 
                                    pattern = "Ivory Coast",
                                    replacement = "Côte d'Ivoire")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Solomon Islands", 
                                    replacement = "Solomon Is.")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "East Timor",
                                    replacement = "Timor-Leste")) %>%
  mutate(country_name = str_replace(country_name, 
                                    pattern = "Korea South",
                                    replacement = "Dem. Rep. Korea")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Korea North",
                                    replacement = "Korea")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "South Sudan",
                                    replacement = "S. Sudan")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Congo Kinshasa",
                                    replacement = "Dem. Rep. Congo")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Congo Brazzaville",
                                    replacement = "Congo")) %>%
  filter(! regime_r == "NA")

# This data set tells me the type of regime within each country.


first_graph <- first_clean %>%
  select(country_name,
         country_code,
         indicator_name,
         x2015:x2018) %>%
  drop_na() %>%
  filter(indicator_name %in% c("Total natural resources rents (% of GDP)",
                               "Tax revenue (% of GDP)")) %>%
  mutate(average = (x2015 + x2016 + x2017 +x2018)/4) %>%
  filter(average > 0) %>%
  pivot_wider(names_from = indicator_name, values_from = average,
              id_cols = c("country_name", "country_code")) %>%
  drop_na() %>%
  clean_names() %>% 
  ggplot(aes(x = total_natural_resources_rents_percent_of_gdp,
             y = tax_revenue_percent_of_gdp)) +
  geom_point(color = "red") +
  geom_smooth(method = lm,
              se = FALSE,
              color = "dodgerblue") +
  labs(x = "Natural Resources Rent as % of GDP",
       y = "Tax Revenue as % of GDP",
       title = "Relationship Between Tax Revenue and Natural Resources Rent as % of GDP",
       subtitle  = "Does Depending on Natural Resources Mean Taxing Less?",
       caption = "Source: World Bank Development Inidicators") +
  theme_linedraw()

# This graph gives me an idea of the relationship between dependence on natural 
# resources and tax revenues as a percentage of GDP.


regime_map <- countrynames_worldmap %>%
  left_join(regime_filter) %>%
  ggplot(aes(fill = regime_r)) +
    geom_sf() +
    scale_fill_discrete(na.translate = FALSE,
                        labels = c("Democracy",
                                   "Military Rule",
                                   "Monarchy",
                                   "Multiparty System",
                                   "Single Party System"),
                        name = "Regime Type") +
    labs(title = "World Map of Regime Types in 2011",
         caption = "Source: https://cddrl.fsi.stanford.edu/
         research/autocracies_of_the_world_dataset")

# A map showing me the different regimes types on a world map.

regime_count <- regime_filter %>%
  group_by(regime_r) %>%
  count() %>%
  rename("Number" = n,
         "Regime Type" = regime_r) %>%
  as_tibble() %>%
  gt() %>%
  tab_header(title = md("**Regime Types in 2011**"),
             subtitle = md("*This only includes 155 countries*")) %>%
  tab_source_note(
    source_note = md("*Source*: https://cddrl.fsi.stanford.edu/research/autocracies_of_the_world_dataset"))
  



d1 <- democracy_inidcator %>%
  rename(democracy_indicator = `Political Regime (OWID based on Polity IV and Wimmer & Min)`,
         country_name = "Entity") %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Republic",
                                    replacement = "Rep.")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Equatorial Guinea",
                                    replacement = "Eq. Guinea")) %>%
  mutate(country_name = str_replace(country_name, 
                                    pattern = "Cote d'Ivoire",
                                    replacement = "Côte d'Ivoire")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Solomon Islands", 
                                    replacement = "Solomon Is.")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Timor",
                                    replacement = "Timor-Leste")) %>%
  mutate(country_name = str_replace(country_name, 
                                    pattern = "South Korea",
                                    replacement = "Dem. Rep. Korea")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "North Korea",
                                    replacement = "Korea")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "South Sudan",
                                    replacement = "S. Sudan")) %>%
  mutate(country_name = str_replace(country_name,
                                    pattern = "Democratic Rep. of Congo",
                                    replacement = "Dem. Rep. Congo")) %>%
  filter(Year %in% c(2010:2015)) %>%
  group_by(country_name) %>% 
  summarise(dem_indicator = mean(democracy_indicator, na.rm = TRUE))


d2 <- fixed_names %>% 
  filter(indicator_name %in% c("Total natural resources rents (% of GDP)",
                               "Tax revenue (% of GDP)")) %>%
  select(country_name, x2010:x2015, indicator_name) %>%
  pivot_longer(cols = x2010:x2015,
               names_to = "Year",
               values_to = "value") %>%
  group_by(country_name, indicator_name) %>%
  summarise(average = mean(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = indicator_name, values_from = average)

d3 <- d2 %>%
  left_join(d1, by = "country_name") %>%
  left_join(regime_filter, by = "country_name") %>%
  rename(tax_rev_perc = `Tax revenue (% of GDP)`,
         resource_perc = `Total natural resources rents (% of GDP)`) 





fit_1 <- stan_glm(resource_perc ~ regime_r + tax_rev_perc + regime_r*tax_rev_perc,
                  data = d3,
                  refresh = 0)


# outcome as resource percentage, input as regime type and % of tax. consider
# gdp growth, population indicator.

model1_table <- fit_1 %>%
  tbl_regression(intercept = TRUE) %>%
  as_gt() %>%
  tab_header(title = "Regression of Natural Resource Dependence", 
             subtitle = "The Effect of Tax Revenues and Regime Type on Resource
                         Dependence")


fit_2 <- stan_glm(tax_rev_perc ~ resource_perc + regime_r,
                  data = d3,
                  refresh = 0)

model2_table <- fit_2 %>%
  tbl_regression(intercept = TRUE) %>%
  as_gt() %>%
  tab_header(title = "Regression of Tax Revenues as a Percentage of GDP", 
             subtitle = "The Effect of Resource Dependence on Tax Revenues") 





  
  
  





