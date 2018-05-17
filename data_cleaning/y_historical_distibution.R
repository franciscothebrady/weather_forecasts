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
library(lubridate)

#### read in events ####
events <- read_csv("weather_forecasts/data/2_events.csv")
# get rid of some duplicated vars (oops!)
events <- events %>% 
  # select the ones that are NOT auto-renamed with a _1 suffix
  select(-contains("_1"))

#### create df of unique stations and event beginning date -- our upper bound for historical weather! ####
events_n_dates <- data.frame(GHCND.ID = events$GHCND.ID, EVENTS.begin_date = events$EVENTS.begin_date, stringsAsFactors = FALSE)
events_n_dates <- events_n_dates %>%
  group_by(GHCND.ID) %>%
  arrange(desc(EVENTS.begin_date), .by_group = TRUE) %>%
  distinct(GHCND.ID, .keep_all = TRUE)

# ghcnd stations with station info
station_list <- rnoaa::ghcnd_stations()

# merge with station list (to tack on begin_date)
event_station_list <- inner_join(station_list, events_n_dates, by = c("id" = "GHCND.ID"))
# filter on prc
event_station_list <- event_station_list %>% filter(element == "PRCP")
rm(station_list)

# convert first and last year into dates to compare to begin_date
event_station_list <- event_station_list %>%
  mutate(min_date = ymd(paste0(first_year,"-01-01")),
         max_date = ymd(paste0(last_year,"-12-31")))

lower_bound <- lubridate::ymd("2000-01-01")

# replace max date with begin_date, the upper bound for historical weather!
# replace min_date with 2000-01-01 unless it's later than that, then just leave it be
event_station_list <- event_station_list %>%
  mutate(min_date = if_else(min_date < lower_bound, lower_bound, min_date),
         max_date = if_else(max_date >= EVENTS.begin_date, EVENTS.begin_date, max_date))

#### get  weather obs from ghcnd stations (this code is mostly from 2-get_n_process_data.R ####
# looks like ghcnd_search has had some bug fixes!

temp_ls <- vector("list", length(event_station_list$id))
for(j in 1:length(event_station_list$id)){
  station_observations <- data.frame(ghcnd_search(stationid = event_station_list$id[j], 
                                                  var = 'prcp',
                                                  date_min = event_station_list$min_date[j], 
                                                  date_max = event_station_list$max_date[j]), 
                                     stringsAsFactors = FALSE)
  temp_ls[[j]] <- station_observations
  print(j)
}

# unlist into dataframe 
historical <- map_df(.x = temp_ls, .f = ~as.data.frame(.x))
# Remove rows with prcp.prcp as NA
station_obs <- historical[!is.na(historical$prcp.prcp),]

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

write_csv(station_obs, "weather_forecasts/data/historical_observations.csv.gz")

# the number of times a precip category happened 
historical_freq <- station_obs %>%
  mutate(year = lubridate::year(station_obs$prcp.date)) %>%
  group_by(prcp.id, year, prcp.prcp) %>%
  summarise(freq = n())

write_csv(historical_freq, "weather_forecasts/data/historical_freqs.csv")


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
