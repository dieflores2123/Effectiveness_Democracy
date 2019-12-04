library(fs)
library(janitor)
library(dplyr)
library(sf)
library(shinythemes)
library(tidyverse)

gdp_percap <- read_csv("raw-data/UNdata_Export_20191204_070705114.zip",
                       col_types = cols(
                         `Country or Area` = col_character(),
                         Year = col_double(),
                         Item = col_character(),
                         Value = col_double()
                       )) %>% clean_names() %>% 
  rename(c("country_or_area" = "entity")) %>% 
  select(entity, year, value)

regime_type <- read_csv("raw-data/political-regime-updated2016.csv",
                        col_types = cols(
                          Entity = col_character(),
                          Code = col_character(),
                          Year = col_double(),
                          `Political Regime (OWID based on Polity IV and Wimmer & Min) (Score)` = col_double()
                        )) %>% 
  clean_names() %>% 
  rename(c("political_regime_owid_based_on_polity_iv_and_wimmer_min_score" = "score")) %>% 
  mutate(democracy = ifelse(score >= 6, 1, 0)) %>% 
  select(entity, year, score, democracy)

infant_mortality <- read_csv("raw-data/child-deaths-igme-data.csv", 
                             col_types = cols(
                               Entity = col_character(),
                               Code = col_character(),
                               Year = col_double(),
                               `Number of under-five deaths` = col_double()
                             )) %>% 
  clean_names() %>% 
  rename(c("number_of_under_five_deaths" = "rate")) %>% 
  select(entity, year, rate)

world_pop <- read_csv("raw-data/population.csv", 
                      col_types = cols(
                        Entity = col_character(),
                        Code = col_character(),
                        Year = col_double(),
                        Population = col_double()
                      )) %>% 
  clean_names() %>% 
  select(entity, year, population)


world_density <- read_csv("raw-data/population-density.csv",
                          col_types = cols(
                            Entity = col_character(),
                            Code = col_character(),
                            Year = col_double(),
                            `Population density (people per sq. km of land area) (people per km² of land area)` = col_double()
                          )) %>% 
  clean_names() %>% 
  rename(c("population_density_people_per_sq_km_of_land_area_people_per_km_of_land_area" = "density")) %>% 
  select(entity, year, density)

growth_democracy <- read_csv("raw-data/numbers-of-autocracies-and-democracies.csv",
                             col_types = cols(
                               Entity = col_character(),
                               Code = col_character(),
                               Year = col_double(),
                               Democracies = col_double(),
                               Autocracies = col_double()
                             )) %>% 
  clean_names %>% 
  select(year, democracies, autocracies)

life_expectancy <- read_csv("raw-data/life-expectancy.csv",
                            col_types = cols(
                              Entity = col_character(),
                              Code = col_character(),
                              Year = col_double(),
                              `Life expectancy (years)` = col_double()
                            )) %>% clean_names() %>% 
  rename(c("life_expectancy_years" = "expectancy")) %>% 
  select(entity, year, expectancy)

happiness_country <- read_csv("raw-data/happiness-cantril-ladder.csv",
                              col_types = cols(
                                Entity = col_character(),
                                Code = col_character(),
                                Year = col_double(),
                                `World Happiness Report 2016 (Cantril Ladder (0=worst; 10=best))` = col_double()
                              )) %>% 
  clean_names() %>% 
  rename(c("world_happiness_report_2016_cantril_ladder_0_worst_10_best" = "cantril_score")) %>% 
  select(entity, year, cantril_score)

hdi <- read_csv("raw-data/human-development-index.csv", 
                col_types = cols(
                  Entity = col_character(),
                  Code = col_character(),
                  Year = col_double(),
                  `((0-1; higher values are better))` = col_double()
                )) %>% 
  clean_names() %>% 
  rename(c("x0_1_higher_values_are_better" = "hdi")) %>% 
  select(entity, year, hdi)

hdi_happy <- merge(hdi, happiness_country, by = c("entity", "year"), all = TRUE)
hdi_happy_le <- merge(hdi_happy, life_expectancy, by = c("entity", "year"), all = TRUE)
hdi_happy_le_d <- merge(hdi_happy_le, world_density, by = c("entity", "year"), all = TRUE)
hdi_happy_le_d_p <- merge(hdi_happy_le_d, world_pop, by = c("entity", "year"), all = TRUE)
hdi_happy_le_d_p_im <- merge(hdi_happy_le_d_p, infant_mortality, by = c("entity", "year"), all = TRUE)
hdi_happy_le_d_p_im_rt <- merge(hdi_happy_le_d_p_im, regime_type, by = c("entity", "year"), all = TRUE)
full_data <- merge(hdi_happy_le_d_p_im_rt, gdp_percap, by = c("entity", "year"), all = TRUE)


saveRDS(object = gdp_percap, file = "democracy-effectiveness/gdp_percap.rds")
saveRDS(object = regime_type, file = "democracy-effectiveness/regime_type.rds")
saveRDS(object = infant_mortality, file = "democracy-effectiveness/infant_mortality.rds")
saveRDS(object = world_pop, file = "democracy-effectiveness/world_pop.rds")
saveRDS(object = world_density, file = "democracy-effectiveness/world_density.rds")
saveRDS(object = growth_democracy, file = "democracy-effectiveness/growth_democracy.rds")
saveRDS(object = life_expectancy, file = "democracy-effectiveness/life_expectancy.rds")
saveRDS(object = happiness_country, file = "democracy-effectiveness/happiness_country.rds")
saveRDS(object = hdi, file = "democracy-effectiveness/hdi.rds")
saveRDS(object = full_data, file = "democracy-effectiveness/full_data.rds")
# So here I loaded my data and saved it as tibbles and because it was relatively clean I did not have to 

