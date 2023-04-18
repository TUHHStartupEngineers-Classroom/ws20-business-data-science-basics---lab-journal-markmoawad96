library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing
library(ggplot2)
url_home <- "https://www.rosebikes.de/fahrr%C3%A4der/mtb"

html_home <- read_html(url_home)

bike_name_tbl_challenge <- html_home %>%
  
  html_nodes('.catalog-category-bikes__title-text') %>%
  
  html_text %>%
  
  str_remove_all("\n") %>%
  
  enframe(name = "position", value = "name")

bike_price_tbl_challenge <- html_home %>%
  
  html_nodes('.catalog-category-bikes__price-title') %>%
  
  html_text %>%
  
  str_remove_all("\n") %>%
  
  enframe(name = "position", value = "price") %>%
  
  na_if("") %>%
  
  mutate(price = price %>% str_remove_all("ab ")) %>%
  
  mutate(price_in_EUR = price %>% str_remove_all("€")) %>%
  
  select(-price) %>% mutate(price_in_EUR = readr::parse_number(.$price_in_EUR))

bike_name_price_tbl <- left_join(bike_name_tbl_challenge, bike_price_tbl_challenge) %>%
  select(-position)
bike_name_price_tbl
ggplot(bike_name_price_tbl, aes(x = name, y = price_in_EUR, color = name)) +
  geom_col() +
  expand_limits(x = 0, y = 0) +
  labs(title = "Bike Model vs Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))