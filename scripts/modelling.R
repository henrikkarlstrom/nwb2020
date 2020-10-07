# This script details the time series modeling process for 
# the Cristin data.

## SETUP ##

# Required libraries
library(tidyverse)
library(tsibble)
library(fable)
library(fable.prophet)
library(imputeTS)

# Read in processed Cristin data and convert to 
# a time series object
nvi_time <- read_csv("./processed_data/coop_share.csv") %>%
  as_tsibble(index = date)

## PREPROCESSING ##

# Before modelling the data must be processed to fit the 
# models. In this case, this means imputing values for the
# missing dates in the Cristin data set.

# There are 80 missing dates and share values (2 % of the 
# initial data set), which should be imputed with reasonable
# values. Missingness is most prevalent in the first year, 
# and so a imputation with the mean of the nearest
# existing values is chosen.

# First, the date gaps are filled and the values set to NA.
nvi_time <- nvi_time %>% 
  fill_gaps(coop_share = NA)

# Then, the missing values are imputed using a simple moving
# average of the two surrounding values using the na_ma() 
# function from the imputeTS package.
nvi_time$coop_share <- na_ma(nvi_time$coop_share, k = 2)


## MODELLING ##

# The model will take in data on the share of international 
# co-publications over time and use its inherent trend and 
# seasonality to predict future development. The model uses 
# only the time characteristics of the data to make 
# predictions, and is unaware of any policy interventions 
# aimed at shifting the composition of co-authorship.

# For such an intervention to plausibly claim to have
# affected co-authorship towards internation co-publication,
# we would expect a shift in the trend pattern of 
# international co-publications occuring some time after
# the introduction of the policy change.


## Procedure ##

# First, we define a point in time where changes in 
# behaviour due to the policy change should start to appear. 
# The policy change was introduced in late 2015. Given the 
# delay in scientific publication patterns, the earliest 
# time changes could be expected to appear is set to 
# January 2017. This gives a six year training window and a
# three year prediction window for the model.
model_window <- nvi_time %>%
  filter(year(date) < 2017)

# Next, we define the model. For univariate time series the
#  Prophet model
# (see Taylor and Letham 2017, 
# doi:10.7287/peerj.preprints.3190v2)
# is a reasonable choice. The model is specified with some
# standard settings for trend and seasonality.
model_def <- model_window %>%
  model(
    prophet = prophet(
      coop_share ~ 
        growth(
          type = "linear",
          changepoint_prior_scale = 0.5
          ) +
        season(
          period = "year", 
          order = 24, 
          prior_scale = 20,
          type = "additive"
          )
      )
    )

# A forecast is made and probability intervals computed
model_fc <- model_def %>%
  forecast(h = "3 years") %>%
  mutate(
    interval80 = hilo(coop_share, 80),
    interval95 = hilo(coop_share, 95)
    ) %>%
  unpack_hilo(cols = c(interval80, interval95))

# Finally, save the model as an R object for use in plotting
saveRDS(
  object = model_fc, 
  file = "./processed_data/ts_model.rds"
  )
