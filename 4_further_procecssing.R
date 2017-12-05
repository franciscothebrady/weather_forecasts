# francisco
# 4 - combine unemployment and events df
# what this script does
# 1. reads in event csv and bls unemployment data
# 2. figure out lags to include in the data
# 3. 

setwd("~/weather_forecasts/")

#  load libraries
library(dplyr)
library(readr)
library(lubridate)
library(reshape2)
library(stringr)
library(tidyr)
# read in 3_econ-vars.csv
events <- read.csv("data/3_econ-vars.csv", header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
# read in bls unemployment figures (note: we can also use the original response_df, saved as county_unemp.csv,
# this ones just a little cleaner)
bls_vars <- read.csv("data/bls_vars.csv", header = TRUE, stringsAsFactors = FALSE)

# figure out what to do next
# options
# 1. read in sheldus data
# 2. figure out lags? 
# 3. ?????
