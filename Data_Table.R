library(data.table)
library(data.table)
url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
covid_data_dt <- fread(url)

## FROM[WHERE, SELECT/ORDER BY/UPDATE, GROUP BY]

#covid_data_dt[i, j, by]

# Example (filter by year, sum cases, group by continent)
covid_data_dt[year == 2019, sum(cases), by = continentExp]