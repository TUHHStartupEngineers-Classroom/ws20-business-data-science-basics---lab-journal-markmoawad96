library(tidyverse)
library(scales)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(dbplyr)
library(maps)

# Importing data


covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

world <- map_data("world") %>%
  mutate(across(region, str_replace_all, "_", " ")) %>%
  mutate(region = case_when(
    
    region == "UK" ~ "United_Kingdom",
    region == "USA" ~ "United_States_of_America",
    region == "Czech_Republic" ~ "Czechia",
    TRUE ~ region
    
  ))

covid_data_tbl %>% 
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    
    countriesAndTerritories == "United Kingdom" ~ "United_Kingdom",
    countriesAndTerritories == "United_States_of_America" ~ "United States of America",
    countriesAndTerritories == "Czechia"~"Czechia",
    TRUE ~ countriesAndTerritories
    
  ))

#manipulation of world data table
world_map <- world %>%
  select(region, long, lat, group) %>%
  rename(countriesAndTerritories = region)


#manipulation of covid data table
covid_modified_data_tbl <- covid_data_tbl %>%
  select(day, month, year, countriesAndTerritories, deaths, popData2019) %>%
  group_by(year, countriesAndTerritories, popData2019) %>%
  summarise(total_death = sum(deaths)) %>%
  ungroup() %>%
  mutate(mortality_rate = (total_death / popData2019) * 100)

#merging data between 2 tables 
All_data_tbl <- left_join(covid_modified_data_tbl,
                          world_map,
                          by = "countriesAndTerritories") %>%
                filter(year == 2020)

#first layer of the map
world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightblue", colour = "black",size = 0.1)

#second layer of the map
ggplot(data = All_data_tbl, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = mortality_rate), color = "blue",size = 0.1) +
  scale_fill_viridis_c(option = "E", alpha = 0.75 )