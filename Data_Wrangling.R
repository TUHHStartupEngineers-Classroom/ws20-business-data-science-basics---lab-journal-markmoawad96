# 1.0 Load libraries ----
library(tidyverse)

library(readxl)

# 2.0 Importing Files ----
bikes_tbl     <- read_excel(path = "DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx")

bike_orderlines_tbl <- read_rds("DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")

# 3.0 Manipulating Data using column operations ----
## 3.1 Separating and renaming columns ----
bikes_tbl <- read_excel(path = "DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx") %>%
                        separate(col = category,
                                 into = c("category.1", "category.2", "category.3"),
                                 sep = " - ") %>%
                       set_names(names(.) %>% str_replace_all("\\.", "_"))
## 3.2 Selecting by column ----
### Selecting by names ----
bikes_tbl %>% select(bike_id, model, model_year)

### Selecting by indices ----
bikes_tbl %>% select(1:3)

### Selecting by select_helpers 1 ----
bikes_tbl %>% select(1, contains("model"))

### Selecting by select_helpers 2 ----
bikes_tbl %>% select(starts_with("model"))

### Selecting specific columns ----
bikes_tbl %>% select(model, price)

### Selecting with logical operators ----
#Selecting char columns
bikes_tbl %>% select(where(is.character))

#Selecting numeric columns
bikes_tbl %>% select(where(is.numeric))

#Selecting non-numeric columns
bikes_tbl %>% select(!where(is.numeric))

## 3.3 Rearranging Columns ----
bikes_tbl %>% select(category_1:category_3, everything())
  
# OR:
bikes_tbl %>% relocate(category_1:category_3)
  
## 3.4 Pull specific column ----
#Pulling price and calculating the mean
bikes_tbl %>% pull(price) %>% mean()

## 3.5 Renaming columns ----
### Using rename() ----
bikes_tbl %>%
  select(model, category_1, category_2, category_3, price) %>% 
  rename(
    Model           = model,
    `Bike Family`   = category_1,
    `Ride Style`    = category_2,
    `Bike Category` = category_3,
    `Price in Euro` = price
  )

### Using set_names() ----
bikes_tbl %>%
  select(model, category_1, category_2, category_3, price) %>% 
  set_names(c("Model", "Bike Family", "Ride Style", "Bike Category", "Price in Euro"))

# An example using str_replace()
#str_to_title function makes the first letter of each word upper case
bikes_tbl %>%
  select(model, category_1, category_2, category_3, price) %>% 
  set_names(names(.) %>% str_replace("_", " ") %>% str_to_title())

# 4.0 Manipulating Data using row operations ----
## 4.1 arranging rows using arrange() ----
bikes_tbl %>% select(model, price) %>%
  arrange(desc(price))

## 4.2 Filtering rows according to some condition ----
bikes_tbl %>% 
  select(model, price) %>%
  filter(price > mean(price))

## 4.3 Filtering and Sorting in one step ----
bikes_tbl %>%
  select(model, price) %>%
  filter((price > 5000) | (price < 1000)) %>%
  arrange(desc(price))

## 4.4 Filtering according to more than one condition ----
bikes_tbl %>%
  select(model, price) %>%
  filter(price > 5000,
         model %>% str_detect("Endurace")
         )

## 4.5 Filtering rows according to a specific condition using %in% ----
bikes_tbl %>%
  filter(category_1 %in% c("Hybrid / City", "E-Bikes"))

## 4.6 Filtering rows where category_2 is "E-Mountain" ----
bikes_tbl %>%
  filter(category_2 == "E-Mountain")

## 4.7 Filtering by negated conditions ----
bikes_tbl %>%
  filter(!category_1 %in% c("Hybrid / City", "E-Bikes"))

bikes_tbl %>%
  filter(!category_2 == "E-Mountain")

## 4.8 Arranging and selecting by indices using slice() ----
# Arranging descendingly
bikes_tbl %>%
  arrange(desc(price)) %>%
  slice(1:5)

# Arranging ascendingly
bikes_tbl %>%
  arrange(price) %>%
  slice(1:5)

## 4.9 Arranging by price descending and Filtering the last 5 rows using nrow() ----
bikes_tbl %>%
  arrange(desc(price)) %>%
  slice((nrow(.)-4):nrow(.))

## 4.10 Getting unique values using distinct() ----
bikes_tbl %>%
  distinct(category_1)

bikes_tbl %>%
  distinct(category_1, category_2)

bikes_tbl %>%
  distinct(category_1, category_2, category_3)

# 5.0 Column Transformation ----
#Performing column/feature-based calculations/transformations

## 5.1 Adding columns using mutate() ----
### Example 1
bike_orderlines_tbl %>% 
  mutate(freight_costs = 2 * weight)

### Example 2
bike_orderlines_tbl %>%
  mutate(price_log = log(total_price)) %>%
  mutate(price_sqrt = total_price^0.5)

## 5.2 Replacing columns using mutate() ----
bike_orderlines_tbl %>%
  mutate(total_price = log(total_price))

## 5.3 Adding rows using mutate and logical condition ----
bike_orderlines_tbl %>%
  mutate(is_strive = model %>% str_to_lower() %>% str_detect("strive")) %>%
  filter(is_strive)

## Binning ----
## 5.4 Adding columns using ntile() ----
bike_orderlines_tbl %>%
  mutate(price_binned = ntile(total_price, 3)) %>% 
  select(total_price, price_binned, everything())

## 5.5 Using case_when() and quantile() ----
bike_orderlines_tbl %>%
  mutate(price_binned = ntile(total_price, 3)) %>%
  mutate(price_binned2 = case_when(
    #quantile function is used to convert from numeric to categoric,
    #as a percentage from the input value, for example:
    #If total_price is greater than 75% of the total price, then
    #the price is high an so on.
    total_price > quantile(total_price, 0.75) ~ "High",
    total_price > quantile(total_price, 0.25) ~ "Medium",
    TRUE ~ "Low" # Everything else
  )) %>% 
  select(total_price, price_binned, price_binned2, everything())

## 5.6 Using case_when() to convert from text to categorical ----
## More flexible binning with case_when() Text to categorical
## Adding a column that equals to “Aeroad”, when model contains “aerorad”,
# “Ultimate”, when model contains “ultimate”,
# and “Not Aeroad or Ultimate” in every other case:
bike_orderlines_tbl %>%
  mutate(bike_type = case_when(
    model %>% str_to_lower() %>% str_detect("aeroad") ~ "Aeroad",
    model %>% str_to_lower() %>% str_detect("ultimate") ~ "Ultimate",
    TRUE ~ "Not Aeroad or Ultimate" # Everything else
  )) %>% 
  select(bike_type, everything())

# 6.0 Summary Calculations ----
## 6.1 Using Summarise function ----
bike_orderlines_tbl %>%
  summarise(
    revenue = sum(total_price)
  )

## 6.2 Using group_by() and summarise() ----
# Example 1
bike_orderlines_tbl %>%
  group_by(category_1) %>%
  summarise(revenue = sum(total_price))

#Example 2
bike_orderlines_tbl %>%
  group_by(category_1, category_2) %>%
  summarise(revenue = sum(total_price)) %>%
  # Always ungroup() after you summarise().
  #Left-over groups will cause difficult-to-detect errors.
    ungroup() %>%
  arrange(desc(revenue))

## 6.3 grouping and summarizing by multiple values ----
bike_orderlines_tbl %>%
  group_by(category_1, category_2) %>%
  summarise(
    count = n(),
    avg   = mean(total_price),
    med   = median(total_price),
    sd    = sd(total_price),
    min   = min(total_price),
    max   = max(total_price)
  ) %>%
  ungroup() %>%
  arrange(desc(count))

## 6.4 Using across() to detect missing values ----
#across() makes it easy to apply the same transformation to multiple columns
#Create total_price column and insert missing values for demonstration
bike_orderlines_missing <- bike_orderlines_tbl %>%
  mutate(total_price = c(rep(NA, 4), total_price[5:nrow(.)]))

# detect missing (absolute)
bike_orderlines_missing %>%
  summarise(across(everything(), ~sum(is.na(.))))

# detect missing (relative)
bike_orderlines_missing %>%
  summarise(across(everything(), ~sum(is.na(.)) / length(.)))

# Handling missing data
bike_orderlines_missing %>%
  filter(!is.na(total_price))

# 7.0 Reshaping/Pivoting ----
#pivot_wider() and pivot_longer()

#Create a tibble with the sales for each category_1 and each bikeshop
bikeshop_revenue_tbl <- bike_orderlines_tbl %>%
  select(bikeshop, category_1, total_price) %>%
  
  group_by(bikeshop, category_1) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  arrange(desc(sales))
## 7.1 pivot_wider() ----
#The wide format is reader-friendly.
#People tend to read data as wide format, where columns are categories 
#and the cell contents are values.
#Make the values of category_1 to columns and make the values to euro format
#(use scales::dollar(x, big.mark = ".", decimal.mark = ",", prefix = "",
#suffix = " €")).

bikeshop_revenue_formatted_tbl <- bikeshop_revenue_tbl %>%
  pivot_wider(names_from  = category_1,
              values_from = sales) %>%
  mutate(
    Mountain = scales::dollar(Mountain, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
    Gravel = scales::dollar(Gravel, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
    Road     = scales::dollar(Road, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
    `Hybrid / City` = scales::dollar(`Hybrid / City`, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
    `E-Bikes` = scales::dollar(`E-Bikes`, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €")
  )

## 7.2 pivot_longer() ----
bikeshop_revenue_formatted_tbl %>%
  pivot_longer(cols           = c(names(.)[2:6]),
               names_to       = "category_1",
               values_to      = "sales",
               values_drop_na = T) %>%
  mutate(sales =  sales %>% str_remove_all("€|\\.") %>% as.double())

# 8.0 Joining and Binding ----
# left_join(), bind_cols() and bind_rows()
order_dates_tbl <- bike_orderlines_tbl %>% select(1:3)
order_items_tbl  <- bike_orderlines_tbl %>% select(1:2,4:8)

## 8.1 left_join() ----
order_dates_tbl %>%
  
# By argument is not necessary here, because both tibbles
#share the same column names
left_join(y = order_items_tbl,
          by = c("order_id" = "order_id",
                 "order_line" = "order_line")
          )
## 8.2 bind_cols() ----
bike_orderlines_tbl %>%
  select(-contains("category")) %>%
  
  bind_cols(
    bike_orderlines_tbl %>% select(category_1)
  )

## 8.3 bind_rows() ----
#Can be useful for splitting a dataset into a training and a test dateset.
#splitting sets
train_tbl <- bike_orderlines_tbl %>%
  slice(1:(nrow(.)/2))

test_tbl <- bike_orderlines_tbl %>%
  slice((nrow(.)/2 + 1):nrow(.))

#binding sets back
train_tbl %>%
  bind_rows(test_tbl)

# 9.0 Splitting & Combining ----
# Perform operations on text columns
# separate() and unite()

#Select order_date and convert it to character.
#Then separate it into year, month and day. Make each column numeric.
#Combine them again using unite() and convert the column back to a
#Date format (as.Date())

bike_orderlines_tbl %>% 
  select(order_date) %>% 
  mutate(order_date = as.character(order_date)) %>%
  
  # separate
  separate(col  = order_date,
           into = c("year", "month", "day"),
           sep  = "-", remove = FALSE) %>%
  
  mutate(
    year  = as.numeric(year),
    month = as.numeric(month),
    day   = as.numeric(day)
  ) %>%
  
  # unite
  unite(order_date_united, year, month, day, sep = "-", remove = FALSE) %>%
  mutate(order_date_united = as.Date(order_date_united))


