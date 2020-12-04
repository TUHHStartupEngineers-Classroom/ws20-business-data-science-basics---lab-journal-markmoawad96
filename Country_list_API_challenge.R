library(httr)
library(jsonlite)
library(dplyr)
library(glue)
my_apikey <- Sys.getenv("pwd")
url <- modify_url(url = "http://dataservice.accuweather.com", path = glue("/locations/v1/topcities/100?apikey={my_apikey}&language=en-us&details=false"))
resp <- GET(url)
country_list <- resp %>% .$content %>% rawToChar() %>% fromJSON()
City <- country_list$EnglishName
Country <- country_list$Country$EnglishName
Region <- country_list$Region$EnglishName
Longitude <- country_list$GeoPosition$Longitude
Latitude <- country_list$GeoPosition$Latitude
Timezone <- country_list$TimeZone$Name
#optional to add GMT_Offset, it's only removed so that printing fits the screen
#GMT_Offset <- country_list$TimeZone$GmtOffset
country_list_as_df <- data.frame(City, Country, Region,
                                  Longitude, Latitude, Timezone)
head(country_list_as_df, 10)