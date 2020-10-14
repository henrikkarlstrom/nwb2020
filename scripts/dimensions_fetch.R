# This script details the query specification for data 
# retrieval from the Dimensions database.

## SETUP ##
library(tidyverse)
library(httr)
library(jsonlite)

# Read in some helper functions
source("./scripts/utils.R")

# Get authentication token from the Dimensions API. This
# requires subscription access to the database.
token <- get_dimensions_token()

# Create data frame with the queries and run them
data <- tibble(
  country = c("NO", "SE", "DK", "FI"),
  full_query = c(
    paste0(
      'search publications where research_org_countries = "',
      country, 
      '" and year in [2011:2019] 
      return year aggregate count'
      )
    ),
  int_query = c(
    paste0(
      'search publications where research_org_countries = "',
      country, 
      '" and year in [2011:2019] 
      and count(research_org_countries) > 1 
      return year aggregate count'
      )
    ),
  full_fetch = map(
    full_query, 
    slowly(
      ~fetch_results(query = ., token = token) %>% 
        .[["year"]], 
      rate = rate_delay(2)
      )
    ),
  int_fetch = map(
    int_query, 
    slowly(
      ~fetch_results(query = ., token = token) %>% 
        .[["year"]], 
      rate = rate_delay(2)
      )
    )
  )

# Unnest the list-columns from the API query and join
# them together to form the final data frame.
coop_data <- data %>% 
  select(country, full_fetch) %>%
  unnest(full_fetch) %>%
  rename("all_pubs" = count) %>%
  left_join(
    data %>% 
      select(country, int_fetch) %>% 
      unnest(int_fetch) %>%
      rename("int_coop" = count),
    by = c("country", "id")
      )

# Save the data
saveRDS(coop_data, "./processed_data/dimensions_data.rds")
  
