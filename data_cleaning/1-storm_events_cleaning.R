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
#    output from FCC API
# 7. writes it all to a csv (without headers--fix this later by just saving headers separately somewhere)

# setwd("~/weather_forecasts/") # uncomment for windows

# load libraries
library(maps)
library(plyr)
library(dplyr)
library(dtplyr)
library(stringr)
library(purrr)
library(rnoaa)
library(bea.R)
library(geosphere)
library(weathermetrics)
library(jsonlite)
library(httr)
library(lubridate)
library(tidyverse)
library(readr)

# grab a list of the storm event files

files <- list.files(path="data/", 
                    pattern="StormEvents_details-ftp_v1.0_d*",
                    full.names = TRUE, recursive = FALSE)

events_clean <- function(){
  
  for(i in 1:length(files)){

  print(paste("Current time is", Sys.time()))
  #-- get severe weather data with damage reports from NOAA
  #-- (https://www.ncdc.noaa.gov/swdi/#Intro)
  events <- readr::read_csv(files[i])
  
  # filter out events outside CONUS (57+), HI (15), and AK(2).
  events <- dplyr::filter(events, STATE_FIPS < 57 & STATE_FIPS > 2 & STATE_FIPS!=15)

  #-- filter for PRCP related events=="Heavy Rain"
  events <- dplyr::filter(events, EVENT_TYPE == "Heavy Rain")
  
  # filter out obs without lat/lon
  events <- dplyr::filter(events, BEGIN_LAT != "NA")

  # deal with strenge montana issue.
  events$BEGIN_LAT[events$STATE=="MONTANA" & events$BEGIN_LAT >= 49] <- 48.999
  
  dim(events)
  # and events outside of CONUS 
  # (minlon, minlat),(maxlon, maxlat) :
  # (-124.848974, 24.396308) - (-66.885444, 49.384358)
  events <- dplyr::filter(events, BEGIN_LAT >=  24 & BEGIN_LAT <= 50, 
                          BEGIN_LON >= -125 & BEGIN_LON <= -67)
  
  print(paste("Working on", events$YEAR[1],"events."))
  
  dim(events)
  # Pad BEGIN_DAY and END_DAY with 0 before merging with
  # respective BEGIN_YEARMONTH and  END_YEARMONTH
  events$BEGIN_DAY <- str_pad(events$BEGIN_DAY, 2, pad = "0")
  events$END_DAY <- str_pad(events$END_DAY, 2, pad = "0")
  
  # Merge YEARMONTH and DAY and convert to numeric for next set of manipulations
  events <- dplyr::mutate(events, 
                          BEGIN_DATE = ymd(paste0(BEGIN_YEARMONTH, BEGIN_DAY)),
                          END_DATE = ymd(paste0(END_YEARMONTH, END_DAY)))
  
  # Convert event time from local to UTC timezone
  events <- dplyr::mutate(events,
                BEGIN_TIME_UTC = 
                  BEGIN_TIME - as.numeric(gsub("[[:alpha:]]", "", CZ_TIMEZONE))*100,
                END_TIME_UTC = 
                  END_TIME - as.numeric(gsub("[[:alpha:]]", "", CZ_TIMEZONE))*100,
                BEGIN_DATE_UTC = 
                  ifelse(BEGIN_TIME_UTC >= 2400, BEGIN_DATE+1, BEGIN_DATE),
                END_DATE_UTC = 
                  ifelse(END_TIME_UTC >= 2400, END_DATE + 1, END_DATE),
                BEGIN_TIME_UTC = 
                  ifelse(BEGIN_TIME_UTC >= 2400, BEGIN_TIME_UTC - 2400, BEGIN_TIME_UTC),
                END_TIME_UTC = 
                  ifelse(END_TIME_UTC >= 2400, END_TIME_UTC - 2400, END_TIME_UTC))
  
  # Pad BEGIN_TIME_UTC and END_TIME_UTC with 0
  events$BEGIN_TIME_UTC <- str_pad(events$BEGIN_TIME_UTC, 4, pad = "0")
  events$END_TIME_UTC <- str_pad(events$END_TIME_UTC, 4, pad = "0")

  # Sum damages and add to storm_events_precip data frame
  damage_magnitude <- cbind(
    strsplit(substr(events$DAMAGE_PROPERTY, nchar(events$DAMAGE_PROPERTY),
                    nchar(events$DAMAGE_PROPERTY)), ""),
    strsplit(substr(events$DAMAGE_CROPS, nchar(events$DAMAGE_CROPS),
                    nchar(events$DAMAGE_CROPS)), ""))
  damage_magnitude <- ifelse(damage_magnitude == "K", 3,
                             ifelse(damage_magnitude == "M", 6, 9))
  damage_numeric <- cbind(
    as.numeric(strsplit(events$DAMAGE_PROPERTY, "[[:alpha:]]")),
    as.numeric(strsplit(events$DAMAGE_CROPS, "[[:alpha:]]")))
  damage_value <- rowSums(damage_numeric * 10^damage_magnitude, na.rm = TRUE)
  events$DAMAGE_VALUE <- damage_value / 1e3
  events$DAMAGE_VALUE.magnitude <- rep(3, length(events$DAMAGE_VALUE))
  events$DAMAGE_VALUE.unit <- rep("USD", length(events$DAMAGE_VALUE))
  rm(damage_magnitude, damage_numeric, damage_value)
  
  # select columns and renames, drop unnamed
  events <- dplyr::select(events,
                          c(EVENTS.begin_date        = BEGIN_DATE,
                            EVENTS.begin_time_UTC    = BEGIN_TIME_UTC,
                            EVENTS.end_date          = END_DATE,
                            EVENTS.end_time_UTC      = END_TIME_UTC,
                            EVENTS.ID                = EVENT_ID,
                            EVENTS.state             = STATE,
                            EVENTS.fips              = STATE_FIPS,
                            EVENTS.type              = EVENT_TYPE,
                            EVENTS.czfips            = CZ_FIPS,
                            EVENTS.czname            = CZ_NAME,
                            EVENTS.wfo               = WFO,
                            EVENTS.begin_lat         = BEGIN_LAT,
                            EVENTS.begin_lon         = BEGIN_LON,
                            EVENTS.end_lat           = END_LAT,
                            EVENTS.end_lon           = END_LON,
                            EVENTS.damage_value      = DAMAGE_VALUE,
                            EVENTS.damage_magnitude  = DAMAGE_VALUE.magnitude,
                            EVENTS.damage_unit       = DAMAGE_VALUE.unit
                          ))
  
  # reorder variables
  events <- events[ , c("EVENTS.begin_date",
                        "EVENTS.begin_time_UTC",
                        "EVENTS.end_date",
                        "EVENTS.end_time_UTC", 
                        "EVENTS.ID", 
                        "EVENTS.state",
                        "EVENTS.fips",
                        "EVENTS.type",
                        "EVENTS.czfips",
                        "EVENTS.czname",
                        "EVENTS.wfo",
                        "EVENTS.begin_lat",
                        "EVENTS.begin_lon",
                        "EVENTS.end_lat",
                        "EVENTS.end_lon",
                        "EVENTS.damage_value",
                        "EVENTS.damage_magnitude",
                        "EVENTS.damage_unit")]
  
  # sanity check to see if lat/lon vars are actually numbers
  print(is.numeric(events$EVENTS.begin_lon))
  print(is.numeric(events$EVENTS.begin_lat))

    # write to csv
  colnames <- names(events)
  write.table(colnames, "data/colnames.csv",
            append = FALSE, sep = ",")
  
  readr::write_csv(events, "data/1_events.csv",
                   append = TRUE, col_names = FALSE)
  
  print(paste("Finished with", lubridate::year(events$EVENTS.begin_date[1]),
              "events."))

  }
}

events_clean()
