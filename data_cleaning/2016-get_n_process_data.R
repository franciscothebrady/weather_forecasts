##
## Get and process data for 2016
##   Evaluating Economic Benefits of Short-Range (6-84 hours)
##   and Extended Forecasts (12-192 hours)
##
## NOTICE: Current draft evaluates extended forecast (24 hrs vs 192 hrs)
##
## Edit history:
## 2017-02-24, initial version [bs, Brian Seok]
## 2017-03-24, minor edits, gdp lags, [fb, francisco brady]
## 2017-06-08, edits to storm event read-in [fb]
## 2017-06-24, cleaned up code, use get_archived_GFSX_MOS function
##             to pull extended MOS forecast [bs]
## 2017-08-22, 2016 version [fb]


#-- set working directory
#setwd("C:/Users/franc/OneDrive/Documents/Research/Weather Forecasts")
setwd("~/weather_forecasts")


#-- load required packages
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


#-- API keys (fb's API keys)
# Putting this here is very bad practice!
noaakey <- "LHfBxDvUCXUreWbNRDPCGEjYfaBLfLGh"
beakey <- "AF498701-0543-490E-B9B3-B850D6166872"


#-- get real GDP by MSA for 2016
#-- (https://www.bea.gov/API/bea_web_service_api_user_guide.htm)
beaSpecs <- list(
  "UserID" = beakey,
  "method" = "GetData",
  "datasetname" = "RegionalProduct",
  "Component" = "RGDP_MAN",
  "IndustryId" = "1",
  "GeoFIPS" = "MSA",
  "Year" = "2016",
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
cbsa_info <- read.csv("data/cbsa_info_2010.csv", stringsAsFactors = FALSE)

#-- add CBSA codes to corresponding city/state in gdp_msa

# Edit gdp_msa$GeoName to match msa_code_list$CBSA.Title
gdp_msa$GeoName <- gsub(" (Metropolitan Statistical Area)", "", gdp_msa$GeoName, fixed = TRUE)

# Find correct MSA code for corresponding GeoName by matching msa_code_list$CBSA.Title
# with gdp_msa$GeoName and merge result into gdp_msa
gdp_msa$GeoFips <- as.numeric(gdp_msa$GeoFips)
gdp_msa <- merge(gdp_msa, cbsa_info, by.x = "GeoFips", by.y = "CBSA.code")

# Tidy gdp_msa
gdp_msa <- gdp_msa[, c(4,1,3,11,7,6,5)]  # rearrange columns
colnames(gdp_msa) <- c("YEAR", # 4
                       "CBSA.code", # 1
                       "CBSA.title", # 3
                       "CBSA.pop_density", # 11
                       "MSA.GDP", # 7
                       "MSA.GPD.magnitude", # 6
                       "MSA.GPD.unit") # 5

# We no longer need msa_code_list so remove it from workspace to save RAM.
rm(cbsa_info)


#-- get severe weather data with damage reports from NOAA
#-- (https://www.ncdc.noaa.gov/swdi/#Intro)

# take storm events data for 2016
# https://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/StormEvents_details-ftp_v1.0_d2015_c20170216.csv.gz
storm_events <- as.data.frame(read.csv("data/StormEvents_details-ftp_v1.0_d2016_c20170816.csv.gz", stringsAsFactors = FALSE))

# filter out events outside CONUS, HI, and AK.
storm_events <- dplyr::filter(storm_events, STATE_FIPS < 57 & STATE_FIPS > 0)

# filter out episodes with missing damage reports
storm_events <- dplyr::filter(storm_events, !(DAMAGE_PROPERTY == "") & !(DAMAGE_CROPS == ""))

#-- filter for PRCP related events=="Heavy Rain"
storm_events_precip <- dplyr::filter(storm_events, EVENT_TYPE == "Heavy Rain")
## change all references to storm_events to storm_events_precip below this

# Tidy storm_events data frame

# Pad BEGIN_DAY and END_DAY with 0 before merging with
# respective BEGIN_YEARMONTH and  END_YEARMONTH
storm_events_precip$BEGIN_DAY <- str_pad(storm_events_precip$BEGIN_DAY, 2, pad = "0")
storm_events_precip$END_DAY <- str_pad(storm_events_precip$END_DAY, 2, pad = "0")

# Merge YEARMONTH and DAY and convert to numeric for next set of manipulations
storm_events_precip <- dplyr::mutate(storm_events_precip,
                                     BEGIN_DATE = as.numeric(paste0(BEGIN_YEARMONTH, BEGIN_DAY)),
                                     END_DATE = as.numeric(paste0(END_YEARMONTH, END_DAY)))

# Convert event time from local to UTC timezone
storm_events_precip <- dplyr::mutate(storm_events_precip,
                                     BEGIN_TIME_UTC = BEGIN_TIME - as.numeric(gsub("[[:alpha:]]", "", CZ_TIMEZONE))*100,
                                     END_TIME_UTC = END_TIME - as.numeric(gsub("[[:alpha:]]", "", CZ_TIMEZONE))*100,
                                     BEGIN_DATE_UTC = ifelse(BEGIN_TIME_UTC >= 2400, ifelse(BEGIN_DATE + 1 == 20160229, 20160301, BEGIN_DATE + 1), BEGIN_DATE),
                                     END_DATE_UTC = ifelse(END_TIME_UTC >= 2400, ifelse(END_DATE + 1 == 20160229, 20160301, END_DATE + 1), END_DATE),
                                     BEGIN_TIME_UTC = ifelse(BEGIN_TIME_UTC >= 2400, BEGIN_TIME_UTC - 2400, BEGIN_TIME_UTC),
                                     END_TIME_UTC = ifelse(END_TIME_UTC >= 2400, END_TIME_UTC - 2400, END_TIME_UTC))

# Pad BEGIN_TIME_UTC and END_TIME_UTC with 0
storm_events_precip$BEGIN_TIME_UTC <- str_pad(storm_events_precip$BEGIN_TIME_UTC, 4, pad = "0")
storm_events_precip$END_TIME_UTC <- str_pad(storm_events_precip$END_TIME_UTC, 4, pad = "0")

# Sum damages and add to storm_events_precip data frame
damage_magnitude <- cbind(
  strsplit(substr(storm_events_precip$DAMAGE_PROPERTY, nchar(storm_events_precip$DAMAGE_PROPERTY), nchar(storm_events_precip$DAMAGE_PROPERTY)), ""),
  strsplit(substr(storm_events_precip$DAMAGE_CROPS, nchar(storm_events_precip$DAMAGE_CROPS), nchar(storm_events_precip$DAMAGE_CROPS)), ""))
damage_magnitude <- ifelse(damage_magnitude == "K", 3, ifelse(damage_magnitude == "M", 6, 9))
damage_numeric <- cbind(
  as.numeric(strsplit(storm_events_precip$DAMAGE_PROPERTY, "[[:alpha:]]")),
  as.numeric(strsplit(storm_events_precip$DAMAGE_CROPS, "[[:alpha:]]")))
damage_value <- rowSums(damage_numeric * 10^damage_magnitude, na.rm = TRUE)
storm_events_precip$DAMAGE_VALUE <- damage_value / 1e3
storm_events_precip$DAMAGE_VALUE.magnitude <- rep(3, length(storm_events_precip$DAMAGE_VALUE))
storm_events_precip$DAMAGE_VALUE.unit <- rep("USD", length(storm_events_precip$DAMAGE_VALUE))
rm(damage_magnitude, damage_numeric, damage_value)

# Rearrange columns
names(storm_events_precip)
storm_events_precip <- storm_events_precip[, c(52,54,53,55, 8:10, 13, 15:17, 30:32, 45:48, 58:60)]
colnames(storm_events_precip) <- c("EVENTS.begin_date", # 52
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
storm_events_precip$EVENTS.begin_date <- as.Date(as.character(storm_events_precip$EVENTS.begin_date), "%Y%m%d")
storm_events_precip$EVENTS.end_date <- as.Date(as.character(storm_events_precip$EVENTS.end_date), "%Y%m%d")


#-- find closest GHCND station of event and merge it in storm_events_precip

# Get list of GHCND stations
ghcnd_station_list <- ghcnd_stations()

# Create temporary data frame formatted for meteo_nearby_stations()
# filtering out events without geo location
temp_df <- data.frame(id = storm_events_precip$EVENTS.ID[!is.na(storm_events_precip$EVENTS.begin_lat)],
                      latitude = storm_events_precip$EVENTS.begin_lat[!is.na(storm_events_precip$EVENTS.begin_lat)],
                      longitude = storm_events_precip$EVENTS.begin_lon[!is.na(storm_events_precip$EVENTS.begin_lat)])

# Find closest MET station near precip event
met_stations <- meteo_nearby_stations(lat_lon_df = temp_df,
                                      station_data = ghcnd_station_list, limit = 1, var = "PRCP",
                                      year_min = 2016, year_max = 2016)
met_stations <- plyr::rbind.fill(met_stations)  # convert list of data frames into data frame
met_stations$EVENTS.ID <- temp_df$id  # add EVENTS.ID for merging with storm_events_precip

colnames(met_stations) <- c("GHCND.ID","GHCND.name","GHCND.lat","GHCND.lon","GHCND.dist_from_event.km","EVENTS.ID")
rm(temp_df)

# Merge MET stations into storm_events_precip
storm_events_precip <- merge(storm_events_precip, met_stations, by = "EVENTS.ID")


#-- get weather stations with MOS runs
#-- (http://www.nws.noaa.gov/mdl/synop/stadrg.php)
# Modified and saved to csv as mos_stations.csv
# We could have used rnoaa::isd_stations() function, but there are fewer MOS stations
# than ISD stations, so when we pull archived MOS data, it will return errors for
# ISD stations that does not have MOS data.
mos_stations <- read.csv("data/mos_stations.csv", stringsAsFactors = FALSE)


#-- find nearest MOS station to event and merge in storm_events_precip

storm_events_precip$MOS.ICAO <- rep(0, length(storm_events_precip$EVENTS.ID))
storm_events_precip$MOS.name <- rep(0, length(storm_events_precip$EVENTS.ID))
storm_events_precip$MOS.lat <- rep(0, length(storm_events_precip$EVENTS.ID))
storm_events_precip$MOS.lon <- rep(0, length(storm_events_precip$EVENTS.ID))
storm_events_precip$MOS.st <- rep(0, length(storm_events_precip$EVENTS.ID))
storm_events_precip$MOS.dist_from_event.km <- rep(0, length(storm_events_precip$EVENTS.ID))

for (j in 1:length(storm_events_precip$EVENTS.ID)) {
  
  dd <- distm(matrix(c(storm_events_precip$EVENTS.begin_lon[j], storm_events_precip$EVENTS.begin_lat[j]), ncol = 2),
              matrix(c(mos_stations$LON, mos_stations$LAT), ncol = 2),
              fun = distHaversine)  # returns distances in meters
  
  nearest_station_index <- which.min(dd)
  
  storm_events_precip$MOS.ICAO[j] <- mos_stations$ICAO[nearest_station_index]
  storm_events_precip$MOS.name[j] <- mos_stations$NAME[nearest_station_index]
  storm_events_precip$MOS.lat[j] <- mos_stations$LAT[nearest_station_index]
  storm_events_precip$MOS.lon[j] <- mos_stations$LON[nearest_station_index]
  storm_events_precip$MOS.st[j] <- mos_stations$ST[nearest_station_index]
  storm_events_precip$MOS.dist_from_event.km[j] <- dd[nearest_station_index]/1e3  # convert to km
}

rm(j, dd, nearest_station_index)


#-- get observation data from ghcnd stations

# Not sure if this is faster, pull each station's relevant obs, than pull
# all data and then searching for matches. For now, let's try the "loop" approach.

# temp_ls <- lapply(1:length(storm_events_precip$EVENTS.ID),
#   function(j) data.frame(
#     ghcnd_search(storm_events_precip$GHCND.ID[j], var = "PRCP",
#                  date_min = storm_events_precip$EVENTS.begin_date[j],
#                  date_max = storm_events_precip$EVENTS.begin_date[j]),
#     stringsAsFactors = FALSE))
# rm(temp_ls)

# sadly have to deal with errors from ghcnd_search()
temp_ls <- vector("list", length(storm_events_precip$EVENTS.ID))
for (j in 1:length(storm_events_precip$EVENTS.ID)) {
  result <- tryCatch({
    data.frame(ghcnd_search(storm_events_precip$GHCND.ID[j], var = "PRCP",
                            date_min = storm_events_precip$EVENTS.begin_date[j],
                            date_max = storm_events_precip$EVENTS.begin_date[j]),
               stringsAsFactors = FALSE)
  }, warning = function(w) {
    return(
      data.frame(prcp.id=storm_events_precip$GHCND.ID[j],
                 prcp.prcp=NA,
                 prcp.date=storm_events_precip$EVENTS.begin_date[j],
                 prcp.mflag="",
                 prcp.qflag="",
                 prcp.sflag="", stringsAsFactors = FALSE)
    )
  }, error = function(e) {
    return(
      data.frame(prcp.id=storm_events_precip$GHCND.ID[j],
                 prcp.prcp=NA,
                 prcp.date=storm_events_precip$EVENTS.begin_date[j],
                 prcp.mflag="",
                 prcp.qflag="",
                 prcp.sflag="", stringsAsFactors = FALSE)
    )
  }, finally = {
    print(j)
  })
  temp_ls[[j]] <- result
}
rm(result)


#-- save workspace to not have to re-create dataset when something goes wrong
#-- for time consuming processes
#save.image("data/snapshot_2017-07-06_2330.RData")
#load("data/snapshot_2017-07-06_2330.RData")


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

# Remove rows with prcp.prcp as NA
station_obs <- station_obs[!is.na(station_obs$prcp.prcp),]

# (Only if station_obs has PRCP values.) Convert units to inches from 0.1mm
station_obs$prcp.prcp <- station_obs$prcp.prcp*0.1
station_obs$prcp.prcp <- convert_precip(station_obs$prcp.prcp, old_metric = "mm", new_metric = "inches", round = 2)

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
# new position (fb)
names(storm_events_precip)[33] <- "GHCND.prcp_cat"


## USE get_archived_GFSX_MOS.R instead! 
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
#                      runtime == paste(obs_date - 1, "00:00:300+00") |
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


#-- save workspace to not have to re-create dataset when something goes wrong
#-- for time consuming processes
#save.image("data/snapshot_2017-07-09_1830.RData")
#load("data/snapshot_2017-07-09_1830.RData")


# load MOS retrival function
source("data_cleaning/get_archived_GFSX_MOS_GZ.R")

#-- beware! nasty hack job below

# collect 5 day ahead forecast on Q12
mos5day12 <- NULL
for (eid in 1:length(storm_events_precip$EVENTS.ID)) {
  print(eid)
  mos_df <- get_archived_GFSX_MOS(storm_events_precip$MOS.ICAO[eid],
                                  format(storm_events_precip$EVENTS.begin_date[eid]-5, "%Y%m%d"),
                                  "00Z")
  if (is.null(mos_df)) {
    mos5day12 <- rbind(mos5day12,
                       cbind.data.frame(
                         index=eid,
                         EVENTS.ID=storm_events_precip$EVENTS.ID[eid],
                         ICAO=storm_events_precip$MOS.ICAO[eid],
                         RTDT=NA,
                         FCDT=as.Date(storm_events_precip$EVENTS.begin_date[eid], tz = "Zulu"),
                         Q12=NA)
    )
  } else {
    print(dplyr::filter(mos_df, FCDT==as.Date(storm_events_precip$EVENTS.begin_date[eid], tz = "Zulu")))
    mos5day12 <- rbind(mos5day12,
                       cbind.data.frame(
                         index=eid,
                         EVENTS.ID=storm_events_precip$EVENTS.ID[eid],
                         dplyr::filter(mos_df, FCDT==as.Date(storm_events_precip$EVENTS.begin_date[eid], tz = "Zulu"))[1:3],
                         Q12=dplyr::filter(mos_df, FCDT==as.Date(storm_events_precip$EVENTS.begin_date[eid], tz = "Zulu"))$Q12)
    )
  }
}

# collect 1 day ahead forecast on Q12
mos1day12 <- NULL
for (eid in 1:length(storm_events_precip$EVENTS.ID)) {
  print(eid)
  mos_df <- get_archived_GFSX_MOS(storm_events_precip$MOS.ICAO[eid],
                                  format(storm_events_precip$EVENTS.begin_date[eid]-1, "%Y%m%d"),
                                  "00Z")
  if (is.null(mos_df)) {
    mos1day12 <- rbind(mos1day12,
                       cbind.data.frame(
                         index=eid,
                         EVENTS.ID=storm_events_precip$EVENTS.ID[eid],
                         ICAO=storm_events_precip$MOS.ICAO[eid],
                         RTDT=NA,
                         FCDT=as.Date(storm_events_precip$EVENTS.begin_date[eid], tz = "Zulu"),
                         Q12=NA)
    )
  } else {
    print(dplyr::filter(mos_df, FCDT==as.Date(storm_events_precip$EVENTS.begin_date[eid], tz = "Zulu")))
    mos1day12 <- rbind(mos1day12,
                       cbind.data.frame(
                         index=eid,
                         EVENTS.ID=storm_events_precip$EVENTS.ID[eid],
                         dplyr::filter(mos_df, FCDT==as.Date(storm_events_precip$EVENTS.begin_date[eid], tz = "Zulu"))[1:3],
                         Q12=dplyr::filter(mos_df, FCDT==as.Date(storm_events_precip$EVENTS.begin_date[eid], tz = "Zulu"))$Q12)
    )
  }
}

# collect 6 day ahead forecast on Q24
mos6day24 <- NULL
for (eid in 1:length(storm_events_precip$EVENTS.ID)) {
  print(eid)
  mos_df <- get_archived_GFSX_MOS(storm_events_precip$MOS.ICAO[eid],
                                  format(storm_events_precip$EVENTS.begin_date[eid]-6, "%Y%m%d"),
                                  "00Z")
  if (is.null(mos_df)) {
    mos6day24 <- rbind(mos6day24,
                       cbind.data.frame(
                         index=eid,
                         EVENTS.ID=storm_events_precip$EVENTS.ID[eid],
                         ICAO=storm_events_precip$MOS.ICAO[eid],
                         RTDT=NA,
                         FCDT=as.Date(storm_events_precip$EVENTS.begin_date[eid], tz = "Zulu"),
                         Q24=NA)
    )
  } else {
    print(dplyr::filter(mos_df, FCDT==as.Date(storm_events_precip$EVENTS.begin_date[eid], tz = "Zulu")))
    mos6day24 <- rbind(mos6day24,
                       cbind.data.frame(
                         index=eid,
                         EVENTS.ID=storm_events_precip$EVENTS.ID[eid],
                         dplyr::filter(mos_df, FCDT==as.Date(storm_events_precip$EVENTS.begin_date[eid], tz = "Zulu"))[1:3],
                         Q24=dplyr::filter(mos_df, FCDT==as.Date(storm_events_precip$EVENTS.begin_date[eid], tz = "Zulu"))$Q24)
    )
  }
}

# collect 2 day ahead forecast on Q24
mos2day24 <- NULL
for (eid in 1:length(storm_events_precip$EVENTS.ID)) {
  print(eid)
  mos_df <- get_archived_GFSX_MOS(storm_events_precip$MOS.ICAO[eid],
                                  format(storm_events_precip$EVENTS.begin_date[eid]-2, "%Y%m%d"),
                                  "00Z")
  if (is.null(mos_df)) {
    mos2day24 <- rbind(mos2day24,
                       cbind.data.frame(
                         index=eid,
                         EVENTS.ID=storm_events_precip$EVENTS.ID[eid],
                         ICAO=storm_events_precip$MOS.ICAO[eid],
                         RTDT=NA,
                         FCDT=as.Date(storm_events_precip$EVENTS.begin_date[eid], tz = "Zulu"),
                         Q24=NA)
    )
  } else {
    print(dplyr::filter(mos_df, FCDT==as.Date(storm_events_precip$EVENTS.begin_date[eid], tz = "Zulu")))
    mos2day24 <- rbind(mos2day24,
                       cbind.data.frame(
                         index=eid,
                         EVENTS.ID=storm_events_precip$EVENTS.ID[eid],
                         dplyr::filter(mos_df, FCDT==as.Date(storm_events_precip$EVENTS.begin_date[eid], tz = "Zulu"))[1:3],
                         Q24=dplyr::filter(mos_df, FCDT==as.Date(storm_events_precip$EVENTS.begin_date[eid], tz = "Zulu"))$Q24)
    )
  }
}

rm(j, eid)

#-- save workspace to not have to re-create dataset when something goes wrong
#-- for time consuming processes
#save.image("data/snapshot_2017-07-09_1939.RData")
#load("data/snapshot_2017-07-09_1939.RData")


#-- merge forecast data to storm_events_precip ID and date
# save.image("data/snapshot_2017-07-26-0023.Rdata")
save.image("data/2016_snapshot_2017-8-21-1730.RData")

rm(ghcnd_station_list, storm_events)
###
mos_q24 <- merge(mos2day24, mos6day24, by.x="index", by.y="index")
mos_q24 <- data.frame(Q24.f2=mos_q24$Q24.x, Q24.f6=mos_q24$Q24.y)
storm_events_precip <- cbind.data.frame(storm_events_precip, mos_q24)
# names(storm_events_precip)[34] <- "Q12.f1"
# names(storm_events_precip)[35] <- "Q12.f5"
rm(mos_q24)

#-- save workspace to not have to re-create dataset when something goes wrong
#-- for time consuming processes
#save.image("data/2016-snapshot_2017-08-16_1108.RData")
#load("data/snapshot_2017-07-11_0456.RData")

### move to analysis script
# storm_events_precip <- dplyr::mutate(storm_events_precip, 
#                                      judge1 = GHCND.prcp_cat - as.numeric(as.character(Q24.f2)), 
#                                      judge2 = GHCND.prcp_cat - as.numeric(as.character(Q24.f6)))
# 
#-- save workspace to not have to re-create dataset when something goes wrong
#-- for time consuming processes
#save.image("data/snapshot_2017-07-14_1416.RData")
# load("data/snapshot_2017-07-14_1416.RData")

### move this to analysis script
# dplyr::summarise(storm_events_precip, 
#                  mean(judge1, na.rm = TRUE), 
#                  mean(judge2, na.rm = TRUE))
# 
# t.test(abs(storm_events_precip$judge1), abs(storm_events_precip$judge2), paired = TRUE)
# 
# plot(storm_events_precip$judge1)
# plot(abs(storm_events_precip$judge2))

### move this to analysis script
# heavy.rain <- dplyr::filter(storm_events_precip, EVENTS.type == "Heavy Rain")
# heavy.rain.x <- heavy.rain$judge1
# heavy.rain.x2 <- heavy.rain$judge2
# heavy.rain.y <- heavy.rain$EVENTS.damage_value* 10^heavy.rain$EVENTS.damage_magnitude
# plot((heavy.rain.x), heavy.rain.y, ylim=c(0, 150000))
# ?plot
# t.test(heavy.rain.x, heavy.rain.x2, paired = TRUE)
# 
# plot(abs(storm_events_precip$judge1), (storm_events_precip$EVENTS.damage_value* 10^storm_events_precip$EVENTS.damage_magnitude))
# test <- lm((storm_events_precip$EVENTS.damage_value* 10^storm_events_precip$EVENTS.damage_magnitude) ~ abs(storm_events_precip$judge1))
# plot(test)
# summary(test)
# table(storm_events_precip$EVENTS.type)

### move this to final data cleaning script
# using the FCC API to match lat/lon to census tracts
# census block conversion API docs here: https://www.fcc.gov/general/census-block-conversions-api
# 
# # install.packages("httr")
# # install.packages("jsonlite")
# library(jsonlite)
# library(httr)
# options(stringsAsFactors = FALSE)
# 
# 
# # following this as an example: http://tophcito.blogspot.com/2015/11/accessing-apis-from-r-and-little-r.html#fn2
# # set up the url and parameters
# 
# url <- "http://data.fcc.gov/api/block/find?format=json"
# 
# 
# latitude <- storm_events_precip$EVENTS.begin_lat
# 
# longitude <- storm_events_precip$EVENTS.begin_lon
# 
# request <- paste0(url, "&latitude=", latitude, "&longitude=", longitude, "&showall=false")
# 
# str(request)
# 
# 
# # change to 678 for 2016 events
# tracts <- data.frame(FIPS = rep(0, 678),
#                      County.FIPS = rep(0, 678),
#                      County.name = rep(0, 678),
#                      State.FIPS = rep(0, 678),
#                      State.code = rep(0, 678),
#                      State.name = rep(0, 678),
#                      status     = rep(0, 678),
#                      executionTime = rep(0, 678))
# 
# nrain <- length(storm_events_precip$EVENTS.begin_lat)
# #  something about the way this request works requires the df setup beforehand so it fills in as.data.frame nicely.
# for (i in 1:nrain) {
#   latitude <- storm_events_precip$EVENTS.begin_lat[i]
#   longitude <- storm_events_precip$EVENTS.begin_lon[i]
#   request <- fromJSON(paste0(url, "&latitude=", latitude, "&longitude=", longitude, "&showall=false"))
#   tracts[i,] <- as.data.frame.list(request)
# }
# 
# tracts
# # formatting tracts df to merge with storm_events_precip
# tracts$County.name <- toupper(tracts$County.name)
# tracts$State.name <- toupper(tracts$State.name)
# tracts$executionTime <- NULL
# tracts$status <- NULL

write.csv(storm_events_precip, "data/2016_storm_events_processed.csv")
