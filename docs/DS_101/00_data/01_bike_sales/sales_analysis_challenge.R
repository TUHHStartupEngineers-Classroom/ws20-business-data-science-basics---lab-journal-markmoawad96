# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)

library(readxl)

# 2.0 Importing Files ----
bikes_tbl_challenge      <- read_excel(path = "/Users/fadyyoussef/Documents/DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx")

orderlines_tbl_challenge <- read_excel("/Users/fadyyoussef/Documents/DS_101/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")

bikeshops_tbl_challenge  <- read_excel("/Users/fadyyoussef/Documents/DS_101/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")


# 3.0 Joining Data ----
left_join(orderlines_tbl_challenge, bikes_tbl_challenge)

left_join(orderlines_tbl_challenge, bikes_tbl_challenge, by = c("product.id" = "bike.id"))

bike_orderlines_joined_tbl_challenge <- orderlines_tbl_challenge %>%
  left_join(bikes_tbl_challenge, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl_challenge, by = c("customer.id" = "bikeshop.id"))

bike_orderlines_joined_tbl_challenge %>% glimpse()

# 4.0 Wrangling Data ----

bike_orderlines_wrangled_tbl_challenge <- bike_orderlines_joined_tbl_challenge %>%

# 4.1 Separate category name
separate(col    = location,
         into   = c("city", "state"),
         sep    = ", ") %>%
  
# 4.2 Add the total price (price * quantity) 
mutate(total.price = price * quantity) %>%

select(-...1, -gender) %>%

select(-ends_with(".id")) %>%

bind_cols(bike_orderlines_joined_tbl_challenge %>% select(order.id)) %>% 

select(order.id, contains("order"), contains("model"), contains("category"),
       price, quantity, total.price,
       everything()) %>%

rename(bikeshop = name) %>%
set_names(names(.) %>% str_replace_all("\\.", "_"))

# 5.0 Business Insights ----
# 5.1 Sales by Location ----

library(lubridate)
# Step 1 - Manipulate
sales_by_location_tbl <- bike_orderlines_wrangled_tbl_challenge %>%
  
# Select columns
select(state, total_price) %>%

# Grouping by state and summarizing sales
group_by(state) %>% 
summarize(sales = sum(total_price)) %>%

# Optional: Add a column that turns the numbers into a currency format 
# (makes it in the plot optically more appealing)
# mutate(sales_text = scales::dollar(sales)) <- Works for dollar values
mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                   decimal.mark = ",", 
                                   prefix = "", 
                                   suffix = " €"))


# Step 2 - Visualize
sales_by_location_tbl %>%
  
# Setup canvas with the columns state (x-axis) and sales (y-axis)
ggplot(aes(x = state, y = sales)) +

# Geometries
geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
geom_label(aes(label = sales_text)) + # Adding labels to the bars
geom_smooth(method = "lm", se = FALSE) + # Adding a trendline

# Formatting
# scale_y_continuous(labels = scales::dollar) + # Change the y-axis. 
# Again, we have to adjust it for euro values
scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                  decimal.mark = ",", 
                                                  prefix = "", 
                                                  suffix = " €")) +
labs(
  title    = "Revenue by location",
  subtitle = "",
  x = "state", # Override defaults for x and y
  y = "Revenue"
) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5.2 Sales by Location and Year ----

# Step 1 - Manipulate
sales_by_location_and_year_tbl <- bike_orderlines_wrangled_tbl_challenge %>%
  
# Select columns and add a year
select(order_date, total_price, state) %>%
mutate(year = year(order_date)) %>%

# Group by and summarize year and state
group_by(year, state) %>%
summarize(sales = sum(total_price)) %>%
ungroup() %>%

# Format $ Text
mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                   decimal.mark = ",", 
                                   prefix = "", 
                                   suffix = " €"))

# Step 2 - Visualize
sales_by_location_and_year_tbl %>%
  
# Set up x, y, fill
ggplot(aes(x = year, y = sales, fill = state)) +

# Geometries
geom_col() + # Run up to here to get a stacked bar plot

# Facet
facet_wrap(~ state) +

# Formatting
scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                  decimal.mark = ",", 
                                                  prefix = "", 
                                                  suffix = " €")) +
labs(
  title = "Revenue by location and year",
  subtitle = "",
  fill = "" # Changes the legend name
) + theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 6.0 Writing Files ----

# 6.1 Excel ----
#install.packages("writexl")
library("writexl")
bike_orderlines_wrangled_tbl %>%
write_xlsx("/Users/fadyyoussef/Documents/DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines_challenge.xlsx")

# 6.2 CSV ----
bike_orderlines_wrangled_tbl %>% 
write_csv("/Users/fadyyoussef/Documents/DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines_challenge.csv")

# 6.3 RDS ----
bike_orderlines_wrangled_tbl %>% 
write_rds("/Users/fadyyoussef/Documents/DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines_challenge.rds")