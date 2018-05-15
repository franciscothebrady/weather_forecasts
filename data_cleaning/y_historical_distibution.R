# francisco 
# what this script does:
# 1. reads in 2_events.csv
# 2. gather historical weather from the stations in 2_events.csv
# 3. puts it into a nice dataframe
# 4. does some calculations
# 5. appends those calculations to 2_events.csv
# 6. spits that out as 3_events.csv for further processing
library(dtplyr)
library(stringr)
library(purrr)
library(rnoaa)
library(bea.R)
library(tidyverse)
library(geosphere)
library(weathermetrics)

#### read in events ####
events <- read_csv("weather_forecasts/data/2_events.csv")
# get rid of some duplicated vars (oops!)
events <- events %>% 
  # select the ones that are NOT auto-renamed with a _1 suffix
  select(-contains("_1"))

#### create df of unique stations and min/max dates to grab observations ####
# no. of unique stations 
unique(events$GHCND.ID)
# max date
lubridate::ceiling_date(max(events$EVENTS.begin_date), unit = "month")
# min date
lubridate::floor_date(min(events$EVENTS.begin_date), unit = "month")

stations <- events %>% 
  select(GHCND.ID, EVENTS.begin_date) %>%
  mutate(date_min = lubridate::floor_date(min(events$EVENTS.begin_date), unit = "month"),
         date_max = lubridate::ceiling_date(max(events$EVENTS.begin_date), unit = "month")) %>%
  distinct(GHCND.ID, date_min, date_max)

#### get  weather obs from ghcnd stations (this code is mostly from 2-get_n_process_data.R ####

#### THIS IS NOT DONE
# logic here needs to either:
# a) create vector of EVERY DAY from 01-01-2010 t0 12-31-2016
#    and check for precip on each day, or
# b) expand dataframe on each date from 01-01-2010 t0 12-31-2016
#    which makes it easier to use only one counting (for j)


# sadly have to deal with errors from ghcnd_search()
temp_ls <- vector("list", length(stations$GHCND.ID))
for (j in 1:length(stations$GHCND.ID)) {
    result <- tryCatch({
    data.frame(ghcnd_search(stations$GHCND.ID[j], var = "PRCP",
                            date_min = stations$date_min[j],
                            date_max = stations$date_min[j]),
               stringsAsFactors = FALSE)
  }, warning = function(w) {
    return(
      data.frame(prcp.id=stations$GHCND.ID[j],
                 prcp.prcp=NA,
                 prcp.date=stations$date_min[j],
                 prcp.mflag="",
                 prcp.qflag="",
                 prcp.sflag="", stringsAsFactors = FALSE)
    )
  }, error = function(e) {
    return(
      data.frame(prcp.id=stations$GHCND.ID[j],
                 prcp.prcp=NA,
                 prcp.date=stations$date_min[j],
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

#### FIX THE ABOVE BEFORE MOVING ON

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





# 
# station_list <- unique(X2_events$GHCND.ID)
# 
# for(i in 1:3){
# test_precip <- rnoaa::meteo_tidy_ghcnd(stationid = station_list[i], 
#                                        var = "prcp", date_min = "2010-01-01",
#                                        date_max = "2015-12-31")
# error could be cuz i need to update package
# check ghcnd_search instead for messier but more accurate results
#}

# test_precip <- test_precip %>%
#   mutate(cat = weathermetrics::convert_precip(prcp*.01, old_metric = "mm", new_metric = "inches", round = 2))
# # need to then change into forecast categories
# test_precip_by_Year <- test_precip %>%
#   mutate(year = lubridate::year(date),
#          month = lubridate::month(date)) %>%
#   na.omit() %>%
#   group_by(year, month, id) %>%
#   # convert prcp into categories
#   # count category frequency by station
#   # ala - summarise(yearly_precip = n()))
# 
# yearly_avgs <- test_precip_by_Year %>%
#   group_by(year, id) %>%
#   summarise(avg_precip = mean(yearly_precip))
