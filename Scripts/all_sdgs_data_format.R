library(dplyr)
library(readr)
library(janitor)
library(tidyr)
library(rnaturalearth)
library(rnaturalearthdata)

source("~/Documents/ECEHH/SDG_Tracker/sdg_useful.R")
source("Scripts/get_iso3.R")

sdg_df <-
  read.csv("Data/All_SDGS_by_Country.csv", na = c("", "NA"), stringsAsFactors = FALSE) %>% clean_names()

table(sdg_df$nature) #shows how the data was collected:
# C = Country Data
# CA = Country Adjusted Data
# E = Estimated Data
# G = Global Monitoring Data
# M = Modelled Data
# N = Non-Relevant

table(sdg_df$x_sex)

unique(sdg_df$geo_area_name[is.na(get_iso(sdg_df$geo_area_name))])



np <- sdg_df %>%
  select(indicator,
         geo_area_name,
         time_period,
         value,
         x_age,
         x_sex,
         x_reporting_type) %>%
  mutate(iso_a3 = get_iso(geo_area_name)) %>% 
  filter(x_reporting_type == "G" & !is.na(iso_a3)) %>%  #removing estimated and modelled data
  select(-x_reporting_type) %>% 
  select(indicator, country = geo_area_name, iso_a3, year = time_period, value, age = x_age,
         sex = x_sex) %>% 
  arrange(indicator, country, year)

most_recent <- np %>%
  group_by(country, iso_a3, indicator) %>%
  summarize(newest_data = max(year)) %>%
  arrange(newest_data)

world <-
  janitor::clean_names(rnaturalearth::ne_countries(scale = "medium", returnclass = "sf"))


data_check <- world %>% 
  left_join(., most_recent, by = "iso_a3")

ggplot(data = data_check)+
  geom_sf(aes(fill = newest_data))+
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

