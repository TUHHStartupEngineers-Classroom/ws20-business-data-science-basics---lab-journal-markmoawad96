# Importing library: ---- 
library(vroom)
library(tidyverse)
library(data.table)
library(tictoc)

# 2.0 Importing Data ----

# Patents: ----

col_types <- list(
  id = col_character(),
  date = col_date("%Y-%m-%d"),
  num_claims = col_double()
)

patent_tbl <- vroom(
  file       = "/Users/fadyyoussef/Documents/DS_101/02_data_wrangling/Patent_data_reduced/patent.tsv",
  delim      = "\t",
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

#Assignee_id = id,
# Assignee: ----

col_types_assignee <- list(
  id = col_character(),
  type = col_character(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "/Users/fadyyoussef/Documents/DS_101/02_data_wrangling/Patent_data_reduced/assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_assignee,
  na         = c("", "NA", "NULL")
)

# Patent assignee: ----

col_types_patent_assignee <- list(
  patent_id = col_character(),
  assignee_id = col_character()
)

patent_assignee_tbl <- vroom(
  file       = "/Users/fadyyoussef/Documents/DS_101/02_data_wrangling/Patent_data_reduced/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent_assignee,
  na         = c("", "NA", "NULL")
)

col_types_uspc <- list(
  patent_id = col_character(),
  mainclass_id = col_number(),
  sequence = col_number()
)


uspc_tbl <- vroom(
  file       = "/Users/fadyyoussef/Documents/DS_101/02_data_wrangling/Patent_data_reduced/uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types_uspc,
  na         = c("", "NA", "NULL")
)


# 3.0 Converting Data Structure ----

setDT(assignee_tbl)
setDT(patent_tbl)
setDT(patent_assignee_tbl)
setDT(uspc_tbl)

# 4.0 DATA WRANGLING ----
# Q1.What US company / corporation has the most patents? 

setnames(assignee_tbl, "id", "assignee_id")

combined_data <- merge(x = patent_assignee_tbl, y = assignee_tbl, by = "assignee_id")


us_patents <- combined_data %>%
  filter(type == 2)%>%
  filter(!is.na(patent_id) || !is.na(organization)) %>%
  select(-type, -assignee_id)%>% 
  group_by(organization) %>%
  count(patent_id) %>%
  select(-patent_id)%>%
  summarise(total = sum(n))%>%
  arrange(desc(total))   

us_top_10 <- us_patents %>% slice(1:10)
us_top_10


# Q2. What US company had the most patents granted in 2019? 


tbl_2 <- patent_tbl %>%   
         separate(col  = date,
         into = c("year", "month", "day"),
          sep  = "-", remove = TRUE) %>%
          mutate(
              month = as.numeric(month)
            )%>%
          filter(month == 01)%>%
          select(-year, -day)

setnames(tbl_2, "id", "patent_id")
combined_data_2 <- merge(x = tbl_2, y = combined_data, by = "patent_id")

us_top_10_2014 <- combined_data_2%>%
                    filter(type == 2)%>%
                    filter(!is.na(patent_id) || !is.na(organization)) %>%
                    select(organization, patent_id) %>%
                    group_by(organization) %>%
                    count(patent_id) %>%   
                    summarise(total_patents = sum(n))%>%
                    arrange(desc(total_patents)) %>% slice(1:10)  
us_top_10_2014

us_top_10_2014_new <- combined_data_2%>%
                        filter(type == 2 & num_claims == 1)%>%
                        filter(!is.na(patent_id) || !is.na(organization)) %>%
                        select(organization, patent_id) %>%
                        group_by(organization) %>%
                        count(patent_id) %>%   
                        summarise(total_patents = sum(n))%>%
                        arrange(desc(total_patents)) %>% slice(1:10)
us_top_10_2014_new
#Q3. What is the most innovative tech sector? 
#For the top 10 companies (worldwide) with the most patents,
#what are the top 5 USPTO tech main classes?

combined_data_3 <- merge(x = uspc_tbl, y = combined_data_2, by = "patent_id")



top10_worlwide_patents <- combined_data_3  %>%
                  filter(!is.na(patent_id) || !is.na(organization))%>%
                  group_by(organization) %>%
                  arrange(desc(mainclass_id)) %>% # set mainclass order first, the result will be sorted automatically 
                  count(patent_id) %>%
                  select(-patent_id)%>%
                  summarise(total_patents_wordwide = sum(n))%>%
                  ungroup() %>%
                  arrange(desc(total_patents_wordwide)) %>% slice(1:10)  

top10_worlwide_patents