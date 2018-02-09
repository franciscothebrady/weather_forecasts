# seasonal adjustment of unemployment
library(ggplot2)
library(lubridate)
library(scales)
library(tidyverse)

# read in unemployment data
unemp <- read.csv("data/bls_vars.csv", stringsAsFactors = FALSE)

# change dates to date class
unemp$date <- as.Date(unemp$date)

# turn into ts object by series
names(unemp)
# first spread data so there's only one date column
unemp.ts <- spread(data = unemp, key = series.id, value = unemp, drop = TRUE)

