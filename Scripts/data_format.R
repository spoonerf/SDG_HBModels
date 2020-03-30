library(dplyr)
library(readr)
library(janitor)
library(tidyr)
library(rnaturalearth)
library(rnaturalearthdata)

source("~/Documents/ECEHH/SDG_Tracker/sdg_useful.R")

no_pov <-
  read.csv("Data/SDG_1_1_1.csv", na = c("", "NA"), stringsAsFactors = FALSE) %>% clean_names()

table(no_pov$nature) #shows how the data was collected:
# E = Estimated data
# G = Global Monitoring Data
# M = Modelled Data

table(no_pov$x_age)


np <- no_pov %>%
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
         sex = x_sex)

most_recent <- np %>%
                  group_by(iso_a3) %>%
                  summarize(newest_data = max(year)) %>%
                  arrange(newest_data)

world <-
  janitor::clean_names(rnaturalearth::ne_countries(scale = "medium", returnclass = "sf"))


data_check <- world %>% 
                left_join(., most_recent, by = "iso_a3")

ggplot(data = data_check)+
  geom_sf(aes(fill = newest_data))+
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

