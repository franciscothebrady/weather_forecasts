# francisco
# putting together brians get_n_process for the sheldus-merged events
library(checkpoint)
# checkpoint("2017-07-04")
library(plyr)
library(dtplyr)
library(stringr)
library(purrr)
library(rnoaa)
library(bea.R)

library(geosphere)
library(weathermetrics)

# read in  the events data.
events <- read.csv("data/4_sheldus.csv", stringsAsFactors = FALSE)
# change begin_date to date class
events$EVENTS.begin_date <- as.Date(events$EVENTS.begin_date)

# cribbing majorly from get_n_process.R, basically i'm going through line
# by line and changing the code so it'll work with the sheldus-events.csv
#-- find closest GHCND station of event and merge it in storm_events_precip

# Get list of GHCND stations
ghcnd_station_list <- ghcnd_stations()

# Create temporary data frame formatted for meteo_nearby_stations()
# filtering out events without geo location
temp_df <- data.frame(id =  events$EVENTS.ID[!is.na(events$EVENTS.begin_lat]), 
                      latitude = events$EVENTS.begin_lat[!is.na(events$EVENTS.begin_lat]), 
                      longitude = 
                        events$EVENTS.begin_lon[!is.na(events$EVENTS.begin_lon)])

# Find closest MET station near precip event
# this is gonna take a while since its pulling from 2010-2016
met_stations <- meteo_nearby_stations(lat_lon_df = temp_df,
                                      station_data = ghcnd_station_list, limit = 1, var = "PRCP",
                                      year_min = 2010, year_max = 2016)
