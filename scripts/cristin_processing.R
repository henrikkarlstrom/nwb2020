# This script details the data processing steps for Cristin
# data used in the forecasting process.

## Required libraries ##

library(tidyverse)
library(lubridate)


### DATA FROM CRISTIN ###

# Data was collected from DUCT, Unit's Tableau Server instance for batch delivery
# of publication data. This resource is not publically 
# available, but all data used in this analysis is also 
# available through the public Cristin API.
# See https://api.cristin.no/v2/doc/index.html for API 
# specification.

## DUCT export specification ##

# Workbook:FOR_data_sted_person_total

# Columns selected:
#   year_reported 
#   result_id
#   date_created
#   international_cooperation

# Filters applied:
#   year: [2011:2019]
#   nvi_status: "J"
#   nvi_level_historical: [1,2]

# Exported as: nvi_id_int_status.csv


## Procedure ##

# Read in DUCT data and clean column names
nvi <- read_csv2("./raw_data/coop_status.csv")

# The date of registration in Cristin does not correspond
# exactly to reported year of publication. 
# Cases where there is more than a year difference 
# (725 of 205 702 rows) between those values should be 
# filtered out to avoid skewing the model.
nvi <- nvi %>% 
  mutate(date = dmy_hms(date_created)) %>%
  filter(
    between(year(date), 2011, 2019),
    !abs(year(date) - year_reported) > 1
    )

# compute date sums and discard surplus columns
nvi <- nvi %>%
  group_by(
    date = date(date),
    international_cooperation
    ) %>%
  summarise(publications = n()) %>%
  mutate(
    coop_share = publications / sum(publications)
    ) %>%
  filter(international_cooperation == "Ja") %>%
  select(date, coop_share) %>%
  ungroup()

# export as .csv file
write_csv(nvi, path = "./processed_data/coop_share.csv")
