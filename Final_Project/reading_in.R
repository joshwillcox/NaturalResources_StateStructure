library(readr)


# https://data.world/resiport/world-development-indicators

first_clean <- read_rds("raw_data/first_clean")

second_clean <- read_rds("raw_data/secondclean")


# https://ourworldindata.org/democracy

democracy_inidcator <- read_csv("raw_data/political-regime-updated2016-distinction-democracies-and-full-democracies.csv", col_types = cols(
  Entity = col_character(),
  Code = col_character(),
  Year = col_double(),
  `Political Regime (OWID based on Polity IV and Wimmer & Min)` = col_double()
))


# https://cddrl.fsi.stanford.edu/research/autocracies_of_the_world_dataset

regime_type <- readxl::read_excel("raw_data/Data_Set.xls")


