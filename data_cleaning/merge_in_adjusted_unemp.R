# francisco 
# read in events and merge with unemployment

library(tidyverse)
library(lubridate)

# read in events 
events <- read_csv("data/2_events.csv")
# read in unemployment
unemp_adju <- read_csv("data/unemp_adju.csv")
# wrangling
unemp_adju <- unemp_adju %>%
  mutate(Year = lubridate::year(date),
         Month = lubridate::month(date))

# read in a list of bls_series_ids
cols_fwf <- readr::fwf_empty("data/bls_series_ids.txt")
series.ids <- read_fwf("data/bls_series_ids.txt", col_positions = cols_fwf)
View(series.ids)
series.ids <- select(series.ids, X3, X4)
names(series.ids) <- c("series.id","county")
# separate states from counties
series.ids <- separate(series.ids, county, c("county","state"), sep = ",", remove = TRUE) 
# Warning message: Too few values at 2 locations: 90, 323 
# this is a census equivalent and DC, fix these.
series.ids$state[323] <- "DC"
series.ids$state[90] <- "AK"

# now we have the ids for all the counties, but they are not in the format to return the 
# Local Area Unemployment series. We need to add "LAU" as a prefix for local area (not seasonally adjusted - U)
# and the suffix "03" for unemployment. more info here: https://www.bls.gov/help/hlpforma.htm#LA
series.ids$series.id <- paste0("LAU",series.ids$series.id,"03")

# construct fips in events
events$full_FIPS <- events$EVENTS.fips * 1000 + events$EVENTS.czfips 
# sprintf("%05d", events$fcc.county.FIPS)

# do some string massaging
series.ids$county <- gsub("\\bCounty\\b", "", series.ids$county)
series.ids$county <- trimws(series.ids$county, which = "both")
series.ids$state <- trimws(series.ids$state, which = "both")
events$EVENTS.czname <- trimws(events$EVENTS.czname, which = "both")
events$state.name <- tolower(events$state.name)
events$EVENTS.state <- tolower(events$EVENTS.state)
events$fcc.county.name <- tolower(events$fcc.county.name)
events$EVENTS.czname <- tolower(events$EVENTS.czname)
events$state.code <- tolower(events$state.code)
#series.ids$county <- tolower(series.ids$county)
series.ids$state <- tolower(series.ids$state)

# select only series.id counties in events dataset to build request
series.ids$full_FIPS <- substring(series.ids$series.id, 6, 10)

# merge on fcc.county fips
events <- merge(events, series.ids, by = "full_FIPS")


#### this code does not work  #####
# test_full <- full_join(events, unemp_adju, by=c("series.id")) 
# test <- test %>% filter(date <= floor_date(EVENTS.begin_date, "month") & 
#                           date <= floor_date(EVENTS.begin_date, "month") %m+% months(1))
# 
# # this yields exactly the same number of obs as before, which i think is not right...
# 
# events <- events %>%
#   # match unemployment time t
#   # if series.id == series.id & time == time
#   mutate(unemp = ifelse(events$series.id == unemp_adju$series.id & 
#                           floor_date(events$EVENTS.begin_date, unit = "months") == unemp_adju$date, 
#                         unemp_adju$unemp, NA),
#          # match unemp time t+1
#          # if series.id == series.id & time t+1 == time t+1
#          unemp_1 = ifelse(events$series.id == unemp_adju$unemp & 
#                             floor_date(add_with_rollback(events$EVENTS.begin_date, months(1)), 
#                                        unit = "months") == unemp_adju$date, 
#                           unemp_adju$unemp, NA),
#          # if series.id == series.id & time t+2 == time t+2
#          unemp_2 = ifelse(events$series.id == unemp_adju$unemp & 
#                             floor_date(add_with_rollback(events$EVENTS.begin_date, months(2)), 
#                                        unit = "months") == unemp_adju$date, 
#                           unemp_adju$unemp, NA),
#          # if series.id == series.id & time t+3 == time t+3
#          unemp_3 = ifelse(events$series.id == unemp_adju$unemp & 
#                             floor_date(add_with_rollback(events$EVENTS.begin_date, months(3)), 
#                                        unit = "months") == unemp_adju$date, 
#                           unemp_adju$unemp, NA),
#          # if series.id == series.id & time t+4 == time t+4
#          unemp_4 = ifelse(events$series.id == unemp_adju$unemp & 
#                             floor_date(add_with_rollback(events$EVENTS.begin_date, months(4)), 
#                                        unit = "months") == unemp_adju$date, 
#                           unemp_adju$unemp, NA),
#          # if series.id == series.id & time t+5 == time t+5
#          unemp_5 = ifelse(events$series.id == unemp_adju$unemp & 
#                             floor_date(add_with_rollback(events$EVENTS.begin_date, months(5)), 
#                                        unit = "months") == unemp_adju$date, 
#                           unemp_adju$unemp, NA),
#          # if series.id == series.id & time t+6 == time t+6
#          unemp_6 = ifelse(events$series.id == unemp_adju$unemp & 
#                             floor_date(add_with_rollback(events$EVENTS.begin_date, months(6)), 
#                                        unit = "months") == unemp_adju$date, 
#                           unemp_adju$unemp, NA)
#          )
# 

#### this code does not work ####         


#### mutate & ifelse ####
### this code does work ####
test_merge <- events %>%
  mutate(Year = lubridate::year(EVENTS.begin_date),
         Month= lubridate::month(EVENTS.begin_date))
test_merge <- full_join(test_merge, unemp_adju, by = c("series.id"="series.id"))

test_merge <- test_merge %>% select(EVENTS.begin_date, date, unemp, everything())
test_merge <- test_merge %>% dplyr::rename(unemp_date = date)

filtered_test_merge <- test_merge %>% 
  filter(unemp_date >= EVENTS.begin_date & unemp_date <= EVENTS.begin_date %m+% months(6))
                                                       
                                                       
##### rewrite this and make it clean ####