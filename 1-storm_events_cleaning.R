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
  
  # Pad BEGIN_DAY and END_DAY with 0 before merging with
  # respective BEGIN_YEARMONTH and  END_YEARMONTH
  events$BEGIN_DAY <- str_pad(events$BEGIN_DAY, 2, pad = "0")
  events$END_DAY <- str_pad(events$END_DAY, 2, pad = "0")
  
  # Merge YEARMONTH and DAY and convert to numeric for next set of manipulations
  events <- dplyr::mutate(events, 
                          BEGIN_DATE = as.numeric(paste0(BEGIN_YEARMONTH, BEGIN_DAY)),
                          END_DATE = as.numeric(paste0(END_YEARMONTH, END_DAY)))
  
  # Convert event time from local to UTC timezone
  ## leap years: 2012, 2016
  # events <- dplyr::mutate(events,
  #                         BEGIN_TIME_UTC = BEGIN_TIME - as.numeric(gsub("[[:alpha:]]", "", CZ_TIMEZONE))*100,
  #                         END_TIME_UTC = END_TIME - as.numeric(gsub("[[:alpha:]]", "", CZ_TIMEZONE))*100,
  #                         BEGIN_DATE_UTC = ifelse(BEGIN_TIME_UTC >= 2400, ifelse(BEGIN_DATE + 1 == 20150229, 20150301, BEGIN_DATE + 1), BEGIN_DATE),
  #                         END_DATE_UTC = ifelse(END_TIME_UTC >= 2400, ifelse(END_DATE + 1 == 20150229, 20150301, END_DATE + 1), END_DATE),
  #                         BEGIN_TIME_UTC = ifelse(BEGIN_TIME_UTC >= 2400, BEGIN_TIME_UTC - 2400, BEGIN_TIME_UTC),
  #                         END_TIME_UTC = ifelse(END_TIME_UTC >= 2400, END_TIME_UTC - 2400, END_TIME_UTC))

  # Pad BEGIN_TIME_UTC and END_TIME_UTC with 0
  events$BEGIN_TIME_UTC <- str_pad(events$BEGIN_TIME_UTC, 4, pad = "0")
  events$END_TIME_UTC <- str_pad(events$END_TIME_UTC, 4, pad = "0")
  
  # Sum damages and add to storm_events_precip data frame
  damage_magnitude <- cbind(
    strsplit(substr(events$DAMAGE_PROPERTY, nchar(events$DAMAGE_PROPERTY),
                    nchar(events$DAMAGE_PROPERTY)), ""),
    strsplit(substr(events$DAMAGE_CROPS, nchar(events$DAMAGE_CROPS),
                    nchar(events$DAMAGE_CROPS)), ""))
  damage_magnitude <- ifelse(damage_magnitude == "K", 3, ifelse(damage_magnitude == "M", 6, 9))
  damage_numeric <- cbind(
    as.numeric(strsplit(events$DAMAGE_PROPERTY, "[[:alpha:]]")),
    as.numeric(strsplit(events$DAMAGE_CROPS, "[[:alpha:]]")))
  damage_value <- rowSums(damage_numeric * 10^damage_magnitude, na.rm = TRUE)
  events$DAMAGE_VALUE <- damage_value / 1e3
  events$DAMAGE_VALUE.magnitude <- rep(3, length(events$DAMAGE_VALUE))
  events$DAMAGE_VALUE.unit <- rep("USD", length(events$DAMAGE_VALUE))
  rm(damage_magnitude, damage_numeric, damage_value)
  
  # Rearrange columns
  names(events)
  events <- events[, c(52,54,53,55, 8:10, 13, 15:17, 30:32, 45:48, 58:60)]
  colnames(events) <- c("EVENTS.begin_date", # 52
                                     "EVENTS.begin_time_UTC", # 54
                                     "EVENTS.end_date", # 53
                                     "EVENTS.end_time_UTC", # 55
                                     "EVENTS.ID", # 8
                                     "EVENTS.state", # 9
                                     "EVENTS.fips", # 10
                                     "EVENTS.type", # 13
                                     "EVENTS.czfips", # 15
                                     "EVENTS.czname", # 16
                                     "EVENTS.WFO", # 17
                                     "EVENTS.flood_cause", # 30
                                     "EVENTS.hurricane_category", # 31
                                     "EVENTS.tornado_F_scale", # 32
                                     "EVENTS.begin_lat", # 45
                                     "EVENTS.begin_lon", # 46
                                     "EVENTS.end_lat", # 47
                                     "EVENTS.end_lon", # 48
                                     "EVENTS.damage_value", # 58
                                     "EVENTS.damage_magnitude", # 59
                                     "EVENTS.damage_unit") # 60
  
  # Convert date to Date format
  events$EVENTS.begin_date <- as.Date(as.character(events$EVENTS.begin_date), "%Y%m%d")
  events$EVENTS.end_date <- as.Date(as.character(events$EVENTS.end_date), "%Y%m%d")
  
  
  # apply FCC Census Block API from: https://www.fcc.gov/general/census-block-conversions-api
  # set up the url and parameters
    url <- "http://data.fcc.gov/api/block/find?format=json"
  
  latitude <- events$EVENTS.begin_lat
  longitude <- events$EVENTS.begin_lon
  request <- paste0(url, "&latitude=", latitude, "&longitude=", longitude, "&showall=false")
  
  str(request)
  
  # use the names of the lists as the names for the df to make
  # not sure if this is going to work, may have to use = length(nrows(events)
  tracts <- data.frame(FIPS = integer(),
                     fcc.county.FIPS = integer(),
                     fcc.county.name = character(),
                     state.FIPS = integer(),
                     state.code = integer(),
                     state.name = character(),
                     status     = character(),
                     executionTime = integer(),
                     stringsAsFactors = FALSE)
  
  nrain <- length(events$EVENTS.begin_lat)
  #  something about the way this request works requires the df setup beforehand so it fills in as.data.frame nicely.
  for (i in 1:nrain) {
    latitude <- events$EVENTS.begin_lat[i]
    longitude <- events$EVENTS.begin_lon[i]
    request <- fromJSON(paste0(url, "&latitude=", latitude, "&longitude=", longitude, "&showall=false"))
    tracts[i,] <- as.data.frame.list(request)
  }
  
  ### sanity check comparing original obs county and FCC API County
  ### not sure how to implement this totally yet, 
  ### something like identical(events$cz-county, events$fcc.county.name)
  ### but identical is sort of strict
  ### could use fips codes
  
  # at this point grab gdp
  #-- get real GDP by MSA for 2015
  #-- (https://www.bea.gov/API/bea_web_service_api_user_guide.htm)
  beaSpecs <- list(
    "UserID" = beakey,
    "method" = "GetData",
    "datasetname" = "RegionalProduct", 
    "Component" = "RGDP_MAN",
    "IndustryId" = "1",
    "GeoFIPS" = "MSA",
    "Year" = "2010,2011,2012,2013,2014,2015,2016",
    "ResultFormat" = "json"
  )
  gdp_msa <- beaGet(beaSpecs, asWide = FALSE)
  rm(beaSpecs)
  
  # i dont think we need the CBSA code matching that was here since we're using the FCC API
  
  # write to a big old file (I think this should be enough?)
  write.csv(events, "data/all_events.csv",
            append = TRUE, sep = ",",
            row.names = FALSE, col.names = TRUE)
})
  