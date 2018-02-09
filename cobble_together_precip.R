# francisco 
# 6 - adding in forecasts
# in this plot i try to read in the forecast data and merge it with the events.

library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

# read in  the events data.
events <- read.csv("data/4_sheldus.csv", stringsAsFactors = FALSE)
names(events)
# read in old events data with MOS forecasts
# we forgot to save the weather forecasts from the get_n_process.R scripts.
# so we are loading .Rdata taken from each year,
# and saving them to csv with their ID numbers (which are unique),
# to merge with the events included in the sheldus dataset.
#2011
load("~/weather_forecasts/data/2011_snapshot_2017-8-1-2315.RData")
# check year and if GHCND.prcp_cat was saved
table(storm_events_precip$EVENTS.begin_date)
table(storm_events_precip$GHCND.prcp_cat)
names(storm_events_precip)

# save as csv
storm_ghcnd_2011 <- storm_events_precip %>% select(GHCND.ID, EVENTS.begin_date, EVENTS.ID,
                                                   EVENTS.fips, EVENTS.begin_lat, EVENTS.begin_lon)
write.csv(storm_ghcnd_2011, "~/weather_forecasts/data/2011_events_ghcnd.csv")
# 2012

# check year and if GHCND.prcp_cat was saved
table(storm_events_precip$EVENTS.begin_date)
table(storm_events_precip$GHCND.prcp_cat)
names(storm_events_precip)

# save as csv
storm_ghcnd_2011 <- storm_events_precip %>% select(GHCND.ID, EVENTS.begin_date, EVENTS.ID,
                                                   EVENTS.fips, EVENTS.begin_lat, EVENTS.begin_lon)
write(storm_ghcnd_2011, "~/weather_forecasts/data/2011_events_ghcnd.csv")
