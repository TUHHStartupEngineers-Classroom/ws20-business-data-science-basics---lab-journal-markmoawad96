library(tidyverse)
library(ggplot2)
covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

covid_data_wrangeled_tbl <- covid_data_tbl %>%
  group_by(countriesAndTerritories, month, year) %>%
  summarise(deaths = sum(deaths)) %>%
  filter(countriesAndTerritories %in% c("France",
                                        "Germany",
                                        "Spain",
                                        "United_Kingdom",
                                        "United_States_of_America"
                                        )
         ) %>% filter(year > 2019) %>%
  mutate(month = case_when(
    month == 1 ~ "January",
    month == 2 ~ "February",
    month == 3 ~ "March",
    month == 4 ~ "April",
    month == 5 ~ "May",
    month == 6 ~ "June",
    month == 7 ~ "July",
    month == 8 ~ "August",
    month == 9 ~ "September",
    month == 10 ~ "October",
    month == 11 ~ "November",
    month == 12 ~ "December"
                              )
        ) %>%
  ungroup() %>%
  rename(Country = countriesAndTerritories)
ggplot(covid_data_wrangeled_tbl, aes(x = month, y = deaths), colors(Country)) +
  geom_line() +
  expand_limits(x = 0, y = 0)