##
## Get and process data for
##   Evaluating Economic Benefits of Short-Range (6-84 hours)
##   and Extended Forecasts (12-192 hours)
##
## NOTICE: Current draft, only evaluating short-range forecasts (24 hrs vs 72 hrs)
##
## Edit history:
## 2017-02-24, initial version [bs, Brian Seok]
## 2017-03-24, minor edits, gdp lags, [fb, francisco brady]
##


#-- set working directory
setwd("C:/Users/franc/OneDrive/Documents/Research/Weather Forecasts")
home_dir <- getwd()


#-- load required packages
library(checkpoint)
checkpoint("2017-03-24")
library(stringr)
library(dtplyr)   # dplyr and data.table combined
library(geosphere)
library(weathermetrics)
library(rnoaa)
library(bea.R)
library(foreach)
#library(doParallel)


#-- set up cluster for parallel processing
#cl <- makeCluster(2)
#registerDoParallel(cl)


#-- API keys
# Putting this here is very bad practice!
noaakey <- "LHfBxDvUCXUreWbNRDPCGEjYfaBLfLGh"
beakey <- "AF498701-0543-490E-B9B3-B850D6166872"


#-- get real GDP by MSA for 2015
#-- (https://www.bea.gov/API/bea_web_service_api_user_guide.htm)
beaSpecs <- list(
  "UserID" = beakey,
  "method" = "GetData",
  "datasetname" = "RegionalProduct", # need to change and check over syntax
  "Component" = "RGDP_MAN",
  "IndustryId" = "1",
  "GeoFIPS" = "MSA",
  "Year" = "2015",
  "ResultFormat" = "json"
)
gdp_msa <- beaGet(beaSpecs, asWide = FALSE)
rm(beaSpecs)


#-- get CBSA code, area, and pop. den. directly from Census.gov
#-- (https://www.census.gov/programs-surveys/popest.html)
#-- (https://www.census.gov/population/metro/data/pop_data.html)
#-- CBSA = MSA + uSA, "u" is micro

# Modified and coverted
# https://www.census.gov/population/metro/files/CBSA%20Report%20Chapter%203%20Data.xls
# to csv as cbsa_info_2010.csv.
cbsa_info <- read.csv(paste0(home_dir, "/data/cbsa_info_2010.csv"), stringsAsFactors = FALSE)


#-- add CBSA codes to corresponding city/state in gdp_msa

# Edit gdp_msa$GeoName to match msa_code_list$CBSA.Title
gdp_msa$GeoName <- gsub(" (Metropolitan Statistical Area)", "", gdp_msa$GeoName, fixed = TRUE)

# Find correct MSA code for corresponding GeoName by matching msa_code_list$CBSA.Title
# with gdp_msa$GeoName and merge result into gdp_msa
gdp_msa <- merge(gdp_msa, cbsa_info, by.x = "GeoName", by.y = "CBSA.title")

# Tidy gdp_msa
gdp_msa <- gdp_msa[, c(4,8,1,11,7,6,5)]  # rearrange columns
colnames(gdp_msa) <- c("YEAR","CBSA.code","CBSA.title","CBSA.pop_density",
                       "MSA.GDP","MSA.GPD.magnitude","MSA.GPD.unit")

# We no longer need msa_code_list so remove it from workspace to save RAM.
rm(cbsa_info)


#-- get severe weather data with damage reports from NOAA
#-- (https://www.ncdc.noaa.gov/swdi/#Intro)

# Modified
# https://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/StormEvents_details-ftp_v1.0_d2015_c20170216.csv.gz
# as storm_events_2015.csv. Filtered out episodes with missing damage reports.
# Filtered out events outside CONUS, HI, and AK.
storm_events <- read.csv("storm_events_2015.csv", stringsAsFactors = FALSE)

# Tidy storm_events data frame

# Pad BEGIN_DAY and END_DAY with 0 before merging with
# respective BEGIN_YEARMONTH and  END_YEARMONTH
storm_events$BEGIN_DAY <- str_pad(storm_events$BEGIN_DAY, 2, pad = "0")
storm_events$END_DAY <- str_pad(storm_events$END_DAY, 2, pad = "0")

# Merge YEARMONTH and DAY and convert to numeric for next set of manipulations
storm_events <- dplyr::mutate(storm_events,
                              BEGIN_DATE = as.numeric(paste0(BEGIN_YEARMONTH, BEGIN_DAY)),
                              END_DATE = as.numeric(paste0(END_YEARMONTH, END_DAY)))

# Convert event time from local to UTC timezone
storm_events <- dplyr::mutate(storm_events,
  BEGIN_TIME_UTC = BEGIN_TIME - as.numeric(gsub("[[:alpha:]]", "", CZ_TIMEZONE))*100,
  END_TIME_UTC = END_TIME - as.numeric(gsub("[[:alpha:]]", "", CZ_TIMEZONE))*100,
  BEGIN_DATE_UTC = ifelse(BEGIN_TIME_UTC >= 2400, ifelse(BEGIN_DATE + 1 == 20150229, 20150301, BEGIN_DATE + 1), BEGIN_DATE),
  END_DATE_UTC = ifelse(END_TIME_UTC >= 2400, ifelse(END_DATE + 1 == 20150229, 20150301, END_DATE + 1), END_DATE),
  BEGIN_TIME_UTC = ifelse(BEGIN_TIME_UTC >= 2400, BEGIN_TIME_UTC - 2400, BEGIN_TIME_UTC),
  END_TIME_UTC = ifelse(END_TIME_UTC >= 2400, END_TIME_UTC - 2400, END_TIME_UTC))

# Pad BEGIN_TIME_UTC and END_TIME_UTC with 0
storm_events$BEGIN_TIME_UTC <- str_pad(storm_events$BEGIN_TIME_UTC, 4, pad = "0")
storm_events$END_TIME_UTC <- str_pad(storm_events$END_TIME_UTC, 4, pad = "0")

# Sum damages and add to storm_events data frame
damage_magnitude <- cbind(
  strsplit(substr(storm_events$DAMAGE_PROPERTY, nchar(storm_events$DAMAGE_PROPERTY), nchar(storm_events$DAMAGE_PROPERTY)), ""),
  strsplit(substr(storm_events$DAMAGE_CROPS, nchar(storm_events$DAMAGE_CROPS), nchar(storm_events$DAMAGE_CROPS)), ""))
damage_magnitude <- ifelse(damage_magnitude == "K", 3, ifelse(damage_magnitude == "M", 6, 9))
damage_numeric <- cbind(
  as.numeric(strsplit(storm_events$DAMAGE_PROPERTY, "[[:alpha:]]")),
  as.numeric(strsplit(storm_events$DAMAGE_CROPS, "[[:alpha:]]")))
damage_value <- rowSums(damage_numeric * 10^damage_magnitude, na.rm = TRUE)
storm_events$DAMAGE_VALUE <- damage_value / 1e3
storm_events$DAMAGE_VALUE.magnitude <- rep(3, length(storm_events$DAMAGE_VALUE))
storm_events$DAMAGE_VALUE.unit <- rep("USD", length(storm_events$DAMAGE_VALUE))
rm(damage_magnitude, damage_numeric, damage_value)

# Rearrange columns
storm_events <- storm_events[, c(26,24,27,25, 8:11, 15:17, 18:21, 28:30)]  # rearrange columns
colnames(storm_events) <- c("EVENTS.begin_date","EVENTS.begin_time_UTC", "EVENTS.end_date", "EVENTS.end_time_UTC",
                            "EVENTS.ID","EVENTS.state","EVENTS.type","EVENTS.WFO",
                            "EVENTS.flood_cause","EVENTS.hurricane_category","EVENTS.tornado_F_scale",
                            "EVENTS.begin_lat","EVENTS.begin_lon","EVENTS.end_lat","EVENTS.end_lon",
                            "EVENTS.damage_value","EVENTS.damage_magnitude","EVENTS.damage_unit")

# Convert date to Date format
storm_events$EVENTS.begin_date <- as.Date(as.character(storm_events$EVENTS.begin_date), "%Y%m%d")
storm_events$EVENTS.end_date <- as.Date(as.character(storm_events$EVENTS.end_date), "%Y%m%d")


#-- find closest GHCND station of event and merge it in storm_events

# Get list of GHCND stations
ghcnd_station_list <- ghcnd_stations()

# Create temporary data frame formatted for meteo_nearby_stations()
# filtering out events without geo location
temp_df <- data.frame(id = storm_events$EVENTS.ID[!is.na(storm_events$EVENTS.begin_lat)],
                      latitude = storm_events$EVENTS.begin_lat[!is.na(storm_events$EVENTS.begin_lat)],
                      longitude = storm_events$EVENTS.begin_lon[!is.na(storm_events$EVENTS.begin_lat)])

# Find closest MET station near precip event
met_stations <- meteo_nearby_stations(lat_lon_df = temp_df,
                                      station_data = ghcnd_station_list, limit = 1, var = "PRCP",
                                      year_min = 2015, year_max = 2015)
met_stations <- plyr::rbind.fill(met_stations)  # convert list of data frames into data frame
met_stations$EVENTS.ID <- temp_df$id  # add EVENTS.ID for merging with storm_events
colnames(met_stations) <- c("GHCND.ID","GHCND.name","GHCND.lat","GHCND.lon","GHCND.dist_from_event.km","EVENTS.ID")
rm(temp_df)

# Merge MET stations into storm_events
storm_events <- merge(storm_events, met_stations, by = "EVENTS.ID")


#-- get weather stations with MOS runs
#-- (http://www.nws.noaa.gov/mdl/synop/stadrg.php)
# Modified and saved to csv as mos_stations.csv
# We could have used rnoaa::isd_stations() function, but there are fewer MOS stations
# than ISD stations, so when we pull archived MOS data, it will return errors for
# ISD stations that does not have MOS data.
mos_stations <- read.csv("mos_stations.csv", stringsAsFactors = FALSE)


#-- find nearest MOS station to event and merge in storm_events

storm_events$MOS.ICAO <- rep(0, length(storm_events$EVENTS.ID))
storm_events$MOS.name <- rep(0, length(storm_events$EVENTS.ID))
storm_events$MOS.lat <- rep(0, length(storm_events$EVENTS.ID))
storm_events$MOS.lon <- rep(0, length(storm_events$EVENTS.ID))
storm_events$MOS.st <- rep(0, length(storm_events$EVENTS.ID))
storm_events$MOS.dist_from_event.km <- rep(0, length(storm_events$EVENTS.ID))

foreach (j = 1:length(storm_events$EVENTS.ID)) %do% {
  
  dd <- distm(matrix(c(storm_events$EVENTS.begin_lon[j], storm_events$EVENTS.begin_lat[j]), ncol = 2),
              matrix(c(mos_stations$LON, mos_stations$LAT), ncol = 2),
              fun = distHaversine)  # returns distances in meters
  
  nearest_station_index <- which.min(dd)
  
  storm_events$MOS.ICAO[j] <- mos_stations$ICAO[nearest_station_index]
  storm_events$MOS.name[j] <- mos_stations$NAME[nearest_station_index]
  storm_events$MOS.lat[j] <- mos_stations$LAT[nearest_station_index]
  storm_events$MOS.lon[j] <- mos_stations$LON[nearest_station_index]
  storm_events$MOS.st[j] <- mos_stations$ST[nearest_station_index]
  storm_events$MOS.dist_from_event.km[j] <- dd[nearest_station_index]/1e3  # convert to km
}

rm(j, dd, nearest_station_index)


#-- filter for PRCP related events
storm_events_precip <- dplyr::filter(storm_events,
                                     EVENTS.type == "Flash Flood" | 
                                       EVENTS.type == "Flood" |
                                       EVENTS.type == "Heavy Rain" |
                                       EVENTS.type == "Debris Flow")


#-- get observation data from ghcnd stations

# Not sure if this is faster, pull each station's relevant obs, than pull
# all data and then searching for matches. For now, let's try the "loop" approach.
temp_ls <- lapply(1:length(storm_events_precip$EVENTS.ID),
  function(j) data.frame(
    ghcnd_search(storm_events_precip$GHCND.ID[j], var = "PRCP",
                 date_min = storm_events_precip$EVENTS.begin_date[j],
                 date_max = storm_events_precip$EVENTS.begin_date[j]),
    stringsAsFactors = FALSE))
rm(temp_ls)

# Hack job. Not sure why I can't just use dplyr::bind_rows()
#station_obs <- dplyr::bind_rows(temp_ls)
station_obs <- data.frame(prcp.id = character(),
                          prcp.prcp = numeric(),
                          prcp.date = as.Date(character()),
                          stringsAsFactors = FALSE)
for (i in 1:length(temp_ls)) {
  station_obs <- rbind(station_obs, temp_ls[[i]][1:3])
}
station_obs$prcp.id <- as.character(station_obs$prcp.id)
station_obs$prcp.prcp <- as.numeric(station_obs$prcp.prcp)
station_obs$prcp.date <- as.Date(station_obs$prcp.date)
rm(i)
#obs_prcp <- meteo_pull_monitors(storm_events_precip$GHCND.ID, var = "PRCP",
#                                date_min = "2015-01-01", date_max = "2015-12-31")

# Remove rows with prcp.prcp as NA
station_obs <- station_obs[!is.na(station_obs$prcp.prcp),]

# (Only if station_obs has PRCP values.) Convert units to inches from 0.1mm
station_obs$prcp.prcp <- station_obs$prcp.prcp*0.1
station_obs$prcp.prcp <- convert_precip(station_obs$prcp.prcp, old_metric = "mm", new_metric = "inches", round = 2)


#-- save workspace to not have to re-create dataset when something goes wrong
#-- for time consuming processes
# save.image("~/R/NOAA/2017-02-28_2330.RData")
load("2017-02-28_2330.RData")

#-- MOS QPF categories
#-- 0 = no precipitation expected;
#-- 1 = 0.01 - 0.09 inches;
#-- 2 = 0.10 - 0.24 inches;
#-- 3 = 0.25 - 0.49 inches;
#-- 4 = 0.50 - 0.99 inches;
#-- 5 = > 1.00 inches.

# Important that the next 6 lines occur in the following sequence, otherwise everything
# ends up being 6.
station_obs <- within(station_obs, prcp.prcp[prcp.prcp >= 2.00] <- 6)
station_obs <- within(station_obs, prcp.prcp[prcp.prcp >= 1.00 & prcp.prcp < 2.00] <- 5)
station_obs <- within(station_obs, prcp.prcp[prcp.prcp >= 0.50 & prcp.prcp < 1.00] <- 4)
station_obs <- within(station_obs, prcp.prcp[prcp.prcp >= 0.25 & prcp.prcp < 0.50] <- 3)
station_obs <- within(station_obs, prcp.prcp[prcp.prcp >= 0.10 & prcp.prcp < 0.25] <- 2)
station_obs <- within(station_obs, prcp.prcp[prcp.prcp >= 0.01 & prcp.prcp < 0.10] <- 1)
station_obs <- within(station_obs, prcp.prcp[prcp.prcp < 0.01] <- 0)


#-- merge station_obs with storm_events_precip by ID and date
storm_events_precip <- merge(storm_events_precip, station_obs,
                             by.x = c("GHCND.ID", "EVENTS.begin_date"),
                             by.y = c("prcp.id", "prcp.date"))
names(storm_events_precip)[30] <- "GHCND.prcp_cat"

write.csv(storm_events, "storm_events_2015.csv")

select(ICAO.names, date, )

## USE parse_archived_GFSX_MOS.R instead! 
# #-- get archived forecast (MOS) from IA State MESONET
# #-- only short-range available
# 
# mos_base_URL <- "https://mesonet.agron.iastate.edu/mos/csv.php"
# 
# mos_gfs_q12 <- array(dim = c(length(storm_events_precip$EVENTS.ID), 4))
# 
# foreach (k = 1:length(storm_events_precip$EVENTS.ID)) %do% {
#   obs_date <- storm_events_precip$EVENTS.begin_date[k]
#   valid_time <- paste(obs_date, "00:00", sep = "%20")
#   mos_param_URL <- paste0("?station=", storm_events_precip$MOS.ICAO[k],
#                           "&valid=", valid_time)
#   mos_URL <- paste0(mos_base_URL, mos_param_URL)
#   mos_header <- read.csv(url(mos_URL), header = FALSE, nrows = 1, stringsAsFactors = FALSE)
#   mos_header$V24 <- "EMPTY"  # adding "missing" column
#   mos_df <- read.csv(url(mos_URL), header = FALSE, skip = 1, stringsAsFactors = FALSE,
#                      col.names = as.vector(mos_header))
#   mos_df$EMPRY <- NULL  # remove "missing" column
#   mos_gfs <- dplyr::filter(mos_df, model == "GFS",
#                      runtime == paste(obs_date - 1, "00:00:00+00") |
#                        runtime == paste(obs_date - 1, "12:00:00+00") |
#                        runtime == paste(obs_date - 3, "00:00:00+00") |
#                        runtime == paste(obs_date - 3, "12:00:00+00"),
#                      ftime == paste(obs_date, "00:00:00+00"))
#   mos_gfs_q12[k,] <- c(ifelse(!is.na(mos_gfs$q12[1]), mos_gfs$q12[1], NA),
#                        ifelse(!is.na(mos_gfs$q12[2]), mos_gfs$q12[2], NA),
#                        ifelse(!is.na(mos_gfs$q12[3]), mos_gfs$q12[3], NA),
#                        ifelse(!is.na(mos_gfs$q12[4]), mos_gfs$q12[4], NA))
# }
# 
# rm(k)
# 
# mos_gfs_q12 <- as.data.frame(mos_gfs_q12)
# colnames(mos_gfs_q12) <- c("q12_1_00","q12_1_12","q12_3_00","q12_3_12")
# storm_events_precip <- cbind(storm_events_precip, mos_gfs_q12)


#-- shutdown all clusters
#stopCluster(cl)
