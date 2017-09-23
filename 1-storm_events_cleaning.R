# francisco 
# storm events cleaning function
# WHAT THIS PROGRAM DOES:
# 1. reads in all the storm event files from 2010-2015
# 2. filters by "heavy rain" events
# 3. filters out events without latitude and longitude entries
# 4. filters out events in Alaska and Hawaii
# 5. uses FCC Census block API to grab state, county FIPS codes,
#    and census block codes.
# 6. performs a check to make sure original storm events data matches
#    output from FCC API (don't know how to do this yet)

setwd("~/weather_forecasts/")

# load libraries
library(plyr)
library(dtplyr)
library(stringr)
library(purrr)
library(rnoaa)
library(bea.R)
library(geosphere)
library(weathermetrics)
library(jsonlite)
library(httr)

#-- API keys (fb's API keys)
# Putting this here is very bad practice!
noaakey <- "LHfBxDvUCXUreWbNRDPCGEjYfaBLfLGh"
beakey <- "AF498701-0543-490E-B9B3-B850D6166872"


# grab a list of the storm event files

files <- list.files(path="data/", pattern="StormEvents_details-ftp_v1.0_d*",
                    full.names = T, recursive = FALSE)

# function to clean observations and append them all into one file at the end.
lapply(files, function(x) {
  #-- get severe weather data with damage reports from NOAA
  #-- (https://www.ncdc.noaa.gov/swdi/#Intro)
  events <- as.data.frame(read.csv(events, stringsAsFactors = FALSE))
  
  # filter out events outside CONUS, HI, and AK.
  events <- dplyr::filter(events, STATE_FIPS < 57 & STATE_FIPS > 0)
  
  #-- filter for PRCP related events=="Heavy Rain"
  events <- dplyr::filter(events, EVENT_TYPE == "Heavy Rain")
  # filter out obs without lat/lon
  events <- dplyr::filter(events, BEGIN_LATITUDE != "NA")
  
  # some data manipulation
  
  
  # apply census block API 
  # from: https://www.fcc.gov/general/census-block-conversions-api
  
  # set up the url and parameters
    url <- "http://data.fcc.gov/api/block/find?format=json"
  
  latitude <- events$EVENTS.begin_lat
  longitude <- events$EVENTS.begin_lon
  request <- paste0(url, "&latitude=", latitude, "&longitude=", longitude, "&showall=false")
  
  str(request)
  
  # use the names of the lists as the names for the df to make
  tracts <- data.frame(FIPS = integer(),
                     County.FIPS = integer(),
                     County.name = character(),
                     State.FIPS = integer(),
                     State.code = integer(),
                     State.name = character(),
                     status     = character(),
                     executionTime = integer(),
                     stringsAsFactors = FALSE)
  
  nrain <- length(storm_events_precip$EVENTS.begin_lat)
  #  something about the way this request works requires the df setup beforehand so it fills in as.data.frame nicely.
  for (i in 1:nrain) {
    latitude <- storm_events_precip$EVENTS.begin_lat[i]
    longitude <- storm_events_precip$EVENTS.begin_lon[i]
    request <- fromJSON(paste0(url, "&latitude=", latitude, "&longitude=", longitude, "&showall=false"))
    tracts[i,] <- as.data.frame.list(request)
  }
  
    
  # sanity check comparing original obs county and FCC API County
  
  # write to a big old file
  write.csv(events, "data/all_events.csv",
            append = TRUE, sep = ",",
            row.names = FALSE, col.names = TRUE)
})
  