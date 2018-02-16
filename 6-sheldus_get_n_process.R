# francisco
# putting together brians get_n_process for the sheldus-merged events
library(checkpoint)
# checkpoint("2017-07-04")
library(plyr)
library(dtplyr)
library(dplyr)
library(stringr)
library(purrr)
library(rnoaa)
library(bea.R)
library(purrr)
library(lubridate)
library(geosphere)
library(weathermetrics)

#### read in  the events data ####
events <- read.csv("data/4_sheldus.csv", stringsAsFactors = FALSE)
# change begin_date to date class
events$EVENTS.begin_date <- as.Date(events$EVENTS.begin_date)

# cribbing majorly from get_n_process.R, basically i'm going through line
# by line and changing the code so it'll work with the sheldus-events.csv

####-- find closest GHCND station of event and merge it in storm_events_precip ####
# Get list of GHCND stations
ghcnd_station_list <- ghcnd_stations()

# Create temporary data frame formatted for meteo_nearby_stations()
# filtering out events without geo location
temp_df <- data.frame(id =  events$EVENTS.ID[!is.na(events$EVENTS.begin_lat)], 
                      latitude = events$EVENTS.begin_lat[!is.na(events$EVENTS.begin_lat)], 
                      longitude = events$EVENTS.begin_lon[!is.na(events$EVENTS.begin_lon)])

#### Find closest MET station near precip event ####
# this is gonna take a while since its pulling from 2010-2016
met_stations <- meteo_nearby_stations(lat_lon_df = temp_df,
                                      station_data = ghcnd_station_list, limit = 1, var = "PRCP",
                                      year_min = 2010, year_max = 2016)

# met_stations <- plyr::rbind.fill(met_stations)  # convert list of data frames into data frame # no longer works
# met_stations$EVENTS.ID <- temp_df$id  # add EVENTS.ID for merging with sheldus events

met_stations <- map_df(met_stations, ~as.data.frame(.x), .id="id")

colnames(met_stations) <- c("EVENTS.ID","GHCND.ID","GHCND.name","GHCND.lat","GHCND.lon",
                            "GHCND.dist_from_event.km")
rm(temp_df)

#### Merge MET stations into sheldus events ####
events <- merge(events, met_stations, by = "EVENTS.ID")

####-- get weather stations with MOS runs ####
#-- (http://www.nws.noaa.gov/mdl/synop/stadrg.php)
# Modified and saved to csv as mos_stations.csv
# We could have used rnoaa::isd_stations() function, but there are fewer MOS stations
# than ISD stations, so when we pull archived MOS data, it will return errors for
# ISD stations that does not have MOS data.
mos_stations <- read.csv("data/mos_stations.csv", stringsAsFactors = FALSE)


####-- find nearest MOS station to event and merge in sheldus events ####
# create empty cols to fill in
events$MOS.ICAO   <- rep(0, length(events$EVENTS.ID))
events$MOS.name   <- rep(0, length(events$EVENTS.ID))
events$MOS.lat    <- rep(0, length(events$EVENTS.ID))
events$MOS.lon    <- rep(0, length(events$EVENTS.ID))
events$MOS.st     <- rep(0, length(events$EVENTS.ID))
events$MOS.dist_from_event.km <- rep(0, length(events$EVENTS.ID))

for (j in 1:length(events$EVENTS.ID)) {
  
  dd <- distm(matrix(c(events$EVENTS.begin_lon[j], events$EVENTS.begin_lat[j]), ncol = 2),
              matrix(c(mos_stations$LON, mos_stations$LAT), ncol = 2),
              fun = distHaversine)  # returns distances in meters
  
  nearest_station_index <- which.min(dd)
  
  events$MOS.ICAO[j]  <- mos_stations$ICAO[nearest_station_index]
  events$MOS.name[j]  <- mos_stations$NAME[nearest_station_index]
  events$MOS.lat[j]   <- mos_stations$LAT[nearest_station_index]
  events$MOS.lon[j]   <- mos_stations$LON[nearest_station_index]
  events$MOS.st[j]    <- mos_stations$ST[nearest_station_index]
  events$MOS.dist_from_event.km[j] <- dd[nearest_station_index]/1e3  # convert to km
}

save.image(paste0("data/", format(now(), "%Y_%m_%d_%H_%M_%S"),".RData"))

rm(j, dd, nearest_station_index)

#####-- get observation data from ghcnd stations ####
# sadly have to deal with errors from ghcnd_search()
temp_ls <- vector("list", length(events$EVENTS.ID))
for (j in 1:length(events$EVENTS.ID)) {
  result <- tryCatch({
    data.frame(ghcnd_search(events$GHCND.ID[j], var = "PRCP",
                            date_min = events$EVENTS.begin_date[j],
                            date_max = events$EVENTS.begin_date[j]),
               stringsAsFactors = FALSE)
  }, warning = function(w) {
    return(
      data.frame(prcp.id=events$GHCND.ID[j],
                 prcp.prcp=NA,
                 prcp.date=events$EVENTS.begin_date[j],
                 prcp.mflag="",
                 prcp.qflag="",
                 prcp.sflag="", stringsAsFactors = FALSE)
    )
  }, error = function(e) {
    return(
      data.frame(prcp.id=events$GHCND.ID[j],
                 prcp.prcp=NA,
                 prcp.date=events$EVENTS.begin_date[j],
                 prcp.mflag="",
                 prcp.qflag="",
                 prcp.sflag="", stringsAsFactors = FALSE)
    )
  }, finally = {
    print(j)
  })
  temp_ls[[j]] <- result
}

#-- save workspace to not have to re-create dataset when something goes wrong
#-- for time consuming processes
save.image(paste0("data/", format(now(), "%Y_%m_%d_%H_%M_%S"),".RData"))

rm(result)

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

### Coding MOS QPF Categories ####
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

####-- merge station_obs with storm_events_precip by ID and date ####
events <- merge(events, station_obs,
                             by.x = c("GHCND.ID", "EVENTS.begin_date"),
                             by.y = c("prcp.id", "prcp.date"))

# names(storm_events_precip)[33] <- "GHCND.prcp_cat"
events <- events %>% rename(GHCND.prcp_cat = prcp.prcp)


#-- save workspace to not have to re-create dataset when something goes wrong
#-- for time consuming processes
save.image(paste0("data/", format(now(), "%Y_%m_%d_%H_%M_%S"),".RData"))
#load("data/snapshot_2017-07-09_1830.RData")

#### USE get_archived_GFSX_MOS.R to get archived MOS forecasts ####
# load MOS retrival function
source("data_cleaning/get_archived_GFSX_MOS.R")

#-- beware! nasty hack job below

#### collect 5 day ahead forecast on Q12 ####
mos5day12 <- NULL
for (eid in 1:length(events$EVENTS.ID)) {
  print(eid)
  mos_df <- get_archived_GFSX_MOS(events$MOS.ICAO[eid],
                                  format(events$EVENTS.begin_date[eid]-5, "%Y%m%d"),
                                  "00Z")
  if (is.null(mos_df)) {
    mos5day12 <- rbind(mos5day12,
                       cbind.data.frame(
                         index=eid,
                         EVENTS.ID=events$EVENTS.ID[eid],
                         ICAO=events$MOS.ICAO[eid],
                         RTDT=NA,
                         FCDT=as.Date(events$EVENTS.begin_date[eid], tz = "Zulu"),
                         Q12=NA)
    )
  } else {
    print(dplyr::filter(mos_df, FCDT==as.Date(events$EVENTS.begin_date[eid], tz = "Zulu")))
    mos5day12 <- rbind(mos5day12,
                       cbind.data.frame(
                         index=eid,
                         EVENTS.ID=events$EVENTS.ID[eid],
                         dplyr::filter(mos_df, FCDT==as.Date(events$EVENTS.begin_date[eid], tz = "Zulu"))[1:3],
                         Q12=dplyr::filter(mos_df,FCDT==as.Date(
                           events$EVENTS.begin_date[eid], tz = "Zulu"))$Q12)
    )
  }
}
save.image(paste0("data/", format(now(), "%Y_%m_%d_%H_%M_%S"),".RData"))
cmd <- '"C:/Program Files/Git/cmd/git.exe" commit -m "uploading 5 day fcst"'
cmd1 <- '"C:/Program Files/Git/cmd/git.exe" push'
system(cmd, intern = T)
system(cmd1, intern = T)
#### collect 1 day ahead forecast on Q12 ####
mos1day12 <- NULL
for (eid in 1:length(events$EVENTS.ID)) {
  print(eid)
  mos_df <- get_archived_GFSX_MOS(events$MOS.ICAO[eid],
                                  format(
                                    events$EVENTS.begin_date[eid]-1, "%Y%m%d"),
                                  "00Z")
  if (is.null(mos_df)) {
    mos1day12 <- rbind(mos1day12,
                       cbind.data.frame(
                         index=eid,
                         EVENTS.ID=events$EVENTS.ID[eid],
                         ICAO=events$MOS.ICAO[eid],
                         RTDT=NA,
                         FCDT=as.Date(events$EVENTS.begin_date[eid], tz = "Zulu"),
                         Q12=NA)
    )
  } else {
    print(dplyr::filter(mos_df, FCDT==as.Date(events$EVENTS.begin_date[eid], tz = "Zulu")))
    mos1day12 <- rbind(mos1day12,
                       cbind.data.frame(
                         index=eid,
                         EVENTS.ID=events$EVENTS.ID[eid],
                         dplyr::filter(mos_df, FCDT==as.Date(events$EVENTS.begin_date[eid], tz = "Zulu"))[1:3],
                         Q12=dplyr::filter(mos_df, FCDT==as.Date(events$EVENTS.begin_date[eid], tz = "Zulu"))$Q12)
    )
  }
}

# collect 6 day ahead forecast on Q24
mos6day24 <- NULL
for (eid in 1:length(events$EVENTS.ID)) {
  print(eid)
  mos_df <- get_archived_GFSX_MOS(events$MOS.ICAO[eid],
                                  format(events$EVENTS.begin_date[eid]-6, "%Y%m%d"),
                                  "00Z")
  if (is.null(mos_df)) {
    mos6day24 <- rbind(mos6day24,
                       cbind.data.frame(
                         index=eid,
                         EVENTS.ID=events$EVENTS.ID[eid],
                         ICAO=events$MOS.ICAO[eid],
                         RTDT=NA,
                         FCDT=as.Date(events$EVENTS.begin_date[eid], tz = "Zulu"),
                         Q24=NA)
    )
  } else {
    print(dplyr::filter(mos_df, FCDT==as.Date(events$EVENTS.begin_date[eid], tz = "Zulu")))
    mos6day24 <- rbind(mos6day24,
                       cbind.data.frame(
                         index=eid,
                         EVENTS.ID=events$EVENTS.ID[eid],
                         dplyr::filter(mos_df, FCDT==as.Date(events$EVENTS.begin_date[eid], tz = "Zulu"))[1:3],
                         Q24=dplyr::filter(mos_df, FCDT==as.Date(events$EVENTS.begin_date[eid], tz = "Zulu"))$Q24)
    )
  }
}

# collect 2 day ahead forecast on Q24
mos2day24 <- NULL
for (eid in 1:length(events$EVENTS.ID)) {
  print(eid)
  mos_df <- get_archived_GFSX_MOS(events$MOS.ICAO[eid],
                                  format(events$EVENTS.begin_date[eid]-2, "%Y%m%d"),
                                  "00Z")
  if (is.null(mos_df)) {
    mos2day24 <- rbind(mos2day24,
                       cbind.data.frame(
                         index=eid,
                         EVENTS.ID=events$EVENTS.ID[eid],
                         ICAO=events$MOS.ICAO[eid],
                         RTDT=NA,
                         FCDT=as.Date(events$EVENTS.begin_date[eid], tz = "Zulu"),
                         Q24=NA)
    )
  } else {
    print(dplyr::filter(mos_df, FCDT==as.Date(events$EVENTS.begin_date[eid], tz = "Zulu")))
    mos2day24 <- rbind(mos2day24,
                       cbind.data.frame(
                         index=eid,
                         EVENTS.ID=events$EVENTS.ID[eid],
                         dplyr::filter(mos_df, FCDT==as.Date(events$EVENTS.begin_date[eid], tz = "Zulu"))[1:3],
                         Q24=dplyr::filter(mos_df, FCDT==as.Date(events$EVENTS.begin_date[eid], tz = "Zulu"))$Q24)
    )
  }
}

####-- save workspace to not have to re-create dataset when something goes wrong ####
#-- for time consuming processes
save.image(paste0("data/", format(now(), "%Y_%m_%d_%H_%M_%S"),".RData"))
#load("data/snapshot_2017-07-09_1939.RData")
rm(j, eid)


rm(ghcnd_station_list, storm_events)

##### merge in forecasts with sheldus events ####
mos_q24 <- merge(mos2day24, mos6day24, by.x="index", by.y="index")
mos_q24 <- data.frame(Q24.f2=mos_q24$Q24.x, Q24.f6=mos_q24$Q24.y)
events <- cbind.data.frame(events, mos_q24)

#### STOP HERE: figure out if dplyr naming conventions ####
#### are still ok ####
# names(storm_events_precip)[34] <- "Q12.f1"
dplyr::rename(events, Q12.f1=mos_q24) # rename by name

# dplyr::rename(events, Q
# names(storm_events_precip)[35] <- "Q12.f5"
rm(mos_q24)
