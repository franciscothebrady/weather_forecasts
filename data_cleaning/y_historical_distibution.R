library(rnoaa)
library(tidyverse)
station_list <- unique(X2_events$GHCND.ID)

for(i in 1:3){
test_precip <- rnoaa::meteo_tidy_ghcnd(stationid = station_list[i], 
                                       var = "prcp", date_min = "2010-01-01",
                                       date_max = "2015-12-31")
# error could be cuz i need to update package
# check ghcnd_search instead for messier but more accurate results
}

test_precip <- test_precip %>%
  mutate(cat = weathermetrics::convert_precip(prcp*.01, old_metric = "mm", new_metric = "inches", round = 2))
# need to then change into forecast categories
test_precip_by_Year <- test_precip %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>%
  na.omit() %>%
  group_by(year, month, id) %>%
  # convert prcp into categories
  # count category frequency by station
  # ala - summarise(yearly_precip = n()))

yearly_avgs <- test_precip_by_Year %>%
  group_by(year, id) %>%
  summarise(avg_precip = mean(yearly_precip))
