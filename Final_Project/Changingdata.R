
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

  ## CREATING THE NECESSARY DATA SETS FOR EASY USE IN MODELS AND GRAPHS

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
  # I got them at different times. The data set I initially got them from was 
  # huge, so there was no chance of uploading it to github.

fixed_names <- fixed_names1 %>%
  rbind(fixed_names2)

  # I bound them together to create the full data set for the project.



world <- ne_countries(scale = "medium",
                      returnclass = "sf")



countrynames_worldmap <- world %>%
  select(name,
         geometry) %>%
  rename(country_name = name)

  # This gives me a data set for all the world's countries and their geometry.
  # I then use it to create all the maps below. When I want to join it with 
  # another data set, I have to put countrynames_worldmap first. If, not it 
  # returns a tibble which is the wrong format for making the map.

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

  # This data set tells me the type of regime within each country for 2011. They 
  # are divided into either Monarchies, Democracies, Military rule, Single Party
  # regimes, and Multiparty regimes. There were a couple put down as "NA". I got 
  # rid of them because I didn't want to code them myself.


 ## THE GRAPHS AND TABLES

# This graph gives me an idea of the relationship between dependence on natural 
# resources and tax revenues as a percentage of GDP.

bestfit_graph <- first_clean %>%
  select(country_name,
         country_code,
         indicator_name,
         x2010:x2015) %>%
  
  # I take a couple of years because it's quite frequent that data isn't 
  # collected every year in some countries. Taking a 6 year gap means 
  # that if only one year has a value, I can use that.
  # Similarly, resources prices 
  # fluctuate a lot and so I wanted to make sure I wasn't using the data from 
  # a year when prices where unusually high or low. The gap is only 6 years
  # , however, in order to make sure nothing to major could have happened.
  # This could have been the case if I'd used a couple decades.
  
  filter(indicator_name %in% c("Total natural resources rents (% of GDP)",
                               "Tax revenue (% of GDP)")) %>%
  select(country_name, x2010:x2015, indicator_name) %>%
  pivot_longer(cols = x2010:x2015,
               names_to = "Year",
               values_to = "value") %>%
  
  # In order to get an average for each indicator over the six years, it needs 
  # to be in a tidy format, hence why I pivot here.
  
  group_by(country_name, indicator_name) %>%
  
  # I want to get a value for each indicator within each country. 
  
  summarise(average = mean(value, na.rm = TRUE), .groups = "keep") %>%
  
  # I put the na.rm argument as TRUE for the reason mentioned earlier. The 
  # government may only record the values once every two years. I don't want to 
  # lose this data and get NA values, so I put this argument in. The .groups 
  # argument gets rid of the error.
  
  pivot_wider(names_from = indicator_name, values_from = average) %>%
  
  # For making a graph, I need it wider again.
  
  drop_na() %>%
  
  # For some countries, I have values for one but NaN for the other. Sadly, I 
  # need to get rid of the NAs for the sake of the graph's format. I end up 
  # losing some important data like Algeria.
  
  clean_names() %>% 
  ggplot(aes(x = total_natural_resources_rents_percent_of_gdp,
             y = tax_revenue_percent_of_gdp)) +
  geom_point(color = "red") +
  geom_smooth(method = lm,
              se = FALSE,
              color = "dodgerblue") +
  
  # I used these colours to make the visual clear.
  
  labs(x = "Natural Resources Rent as % of GDP",
       y = "Tax Revenue as % of GDP",
       title = "Relationship Between Tax Revenue and 
                Natural Resources Rent as % of GDP (2010-2015)",
       subtitle  = "Does Depending on Natural Resources Mean Taxing Less?",
       caption = "Source: World Bank Development Inidicators") +
  theme_linedraw()



# A map showing me the different regimes types on a world map. I understand 
# that I have only done on year (and the year just when the Arab spring was 
# starting so regimes may have been changing). Despite that, it was hard to 
# get an 'average' regime because they were characters. While changes may have 
# been happening, I think it is still useful for visualisation purposes.

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


# This table gives me a count of all the regimes in my data set. It is important
# for the next tab where I talk about limitations. There are only so many 
# monarchies in the world and only so many single party regimes. The data set 
# is hardly large enough to make concrete conclusions. 

regime_count <- regime_filter %>%
  group_by(regime_r) %>%
  count() %>%
  rename("Number" = n,
         "Regime Type" = regime_r) %>%
  as_tibble() %>%
  gt() %>%
  tab_header(title = md("**Regime Types in 2011**"),
             subtitle = md("*This only includes 155 countries*")) %>%
  
  # I found out on some help sites that usig the asteriks makes the font bold or
  # in italics
  
  tab_source_note(
    source_note = md("*Source*: https://cddrl.fsi.stanford.edu/
                     research/autocracies_of_the_world_dataset"))
   


  ## FOR THE MODEL


  # d1 and d2 are joined together and then used as the data set for the 
  # stan_glm. d1  is the data with regime types and d2 is the data with the 
  # indicators for tax revenue and natural resources as a percentage of GDP.

d1 <- democracy_inidcator %>%
  rename(democracy_indicator =
           `Political Regime (OWID based on Polity IV and Wimmer & Min)`,
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
  
  # I again had to the same thing  where I changed the country names 
  # so that everything is in the same format. 
  
  filter(Year %in% c(2010:2015)) %>%
  
  # It's important that I use the same years throughout. 
  
  group_by(country_name) %>% 
  summarise(dem_indicator = mean(democracy_indicator,
                                 na.rm = TRUE),
            .groups = "keep")

 
  # The purpose of d2 is mentioned above.

d2 <- fixed_names %>% 
  filter(indicator_name %in% c("Total natural resources rents (% of GDP)",
                               "Tax revenue (% of GDP)")) %>%
  select(country_name, x2010:x2015, indicator_name) %>%
  pivot_longer(cols = x2010:x2015,
               names_to = "Year",
               values_to = "value") %>%
  group_by(country_name, indicator_name) %>%
  summarise(average = mean(value, 
                           na.rm = TRUE)) %>%
  pivot_wider(names_from = indicator_name, values_from = average)


  # d3 is when I put together regime type, dem_indicator, and my revenue  
  # indicators all into one tibble. This is what I then use for my models. I 
  # start with d2 because it has the most data and I don't want to lose any of
  # it. 
 

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

newdata1a <- tibble(regime_r = c("Monarchy", "Multiparty"),
                   tax_rev_perc = 0)

newdata1b <- tibble(regime_r = c("Monarchy", "Multiparty"),
                   tax_rev_perc = 10)

newdata1c <- tibble(regime_r = c("Monarchy", "Multiparty"),
                    tax_rev_perc = 20)

posteriorform1a <- posterior_epred(fit_1, newdata = newdata1a) %>%
  as_tibble() %>%
  rename("Monarchy" = `1`,
         "Multiparty" = `2`) %>%
  pivot_longer(names_to = "Regime",
               values_to = "predicted",
               cols = everything()) %>%
  ggplot(aes(x = predicted, fill = Regime)) +
  geom_histogram(aes(y = after_stat(count/sum(count))),
                 bins = 100,
                 alpha = 0.7,
                 position = "identity") +
  scale_fill_manual(values = c("red2", "royalblue2")) +
  labs(title = "Predicted Resource Revenue as a % of GDP",
       subtitle = "This applies for when tax revenues as % of GDP  = 0",
       y = "Probability",
       x = "Natural Resource rent (% of GDP)")

posteriorform1b <- posterior_epred(fit_1, newdata = newdata1b) %>%
  as_tibble() %>%
  rename("Monarchy" = `1`,
         "Multiparty" = `2`) %>%
  pivot_longer(names_to = "Regime",
               values_to = "predicted",
               cols = everything()) %>%
  ggplot(aes(x = predicted, fill = Regime)) +
  geom_histogram(aes(y = after_stat(count/sum(count))),
                 bins = 100,
                 alpha = 0.7,
                 position = "identity") +
  scale_fill_manual(values = c("red2", "royalblue2")) +
  labs(title = "Predicted Resource Revenue as a % of GDP",
       subtitle = "This applies for when tax revenues as % of GDP  = 10",
       y = "Probability",
       x = "Natural Resource rent (% of GDP)")

posteriorform1c <- posterior_epred(fit_1, newdata = newdata1c) %>%
  as_tibble() %>%
  rename("Monarchy" = `1`,
         "Multiparty" = `2`) %>%
  pivot_longer(names_to = "Regime",
               values_to = "predicted",
               cols = everything()) %>%
  ggplot(aes(x = predicted, fill = Regime)) +
  geom_histogram(aes(y = after_stat(count/sum(count))),
                 bins = 100,
                 alpha = 0.8,
                 position = "identity") +
  scale_fill_manual(values = c("red2", "royalblue2")) +
  labs(title = "Predicted Resource Revenue as a % of GDP",
       subtitle = "This applies for when tax revenues as % of GDP  = 20",
       y = "Probability",
       x = "Natural Resource rent (% of GDP)")

fit_2 <- stan_glm(dem_indicator ~ resource_perc,
                  data = d3,
                  refresh = 0)

model2_table <- fit_2 %>%
  tbl_regression(intercept = TRUE) %>%
  as_gt() %>%
  tab_header(title = "Regression of Democracy Indicator", 
             subtitle = "The Effect of Resource Dependence on Tax Revenues")
  

newdata2 <- tibble(resource_perc = c(0, 10, 20, 30, 40))

posteriorform2 <- posterior_epred(fit_2, newdata = newdata2) %>%
  as_tibble() %>%
  rename("0" = `1`,
         "10" = `2`,
         "20" = `3`,
         "30" = `4`,
         "40" = `5`) %>%
  pivot_longer(names_to = "resource_perc",
               values_to = "predicted",
               cols = everything()) %>%
  ggplot(aes(x = predicted, fill = resource_perc)) +
  geom_histogram(aes(y = after_stat(count/sum(count))),
                 bins = 300,
                 alpha = 0.8,
                 position = "identity") +
  scale_fill_manual(values = c("red2", "royalblue2", "yellow2", "springgreen2",
                               "slateblue2")) +
  labs(title = "Posterior Prediction for Democracy Rating",
       y = "Probability",
       x = "Predicted Democracy Rating (-10 to 10)",
       fill = "Natural Resource Revenue \n (As % of GDP)") +
  theme_linedraw()


  
  





