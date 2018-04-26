# francisco 
# read in events and merge with unemployment

library(tidyverse)
library(lubridate)

# read in events 
events <- read_csv("data/2_events.csv")
# remove unemployment variable because it's not seasonally adjusted
events <- events %>% select(-unemp) 
# read in unemployment
unemp_adju <- read_csv("data/unemp_adju.csv")
# wrangling
unemp_adju <- unemp_adju %>%
  mutate(Year = lubridate::year(date),
         Month = lubridate::month(date))

#### Attempt 1: full join then filter ####
# merge unemp_adju into events by date, series.
# but ALSO keep unemp up to 6+ months after the event date
test_full <- full_join(events, unemp_adju, by=c("series.id")) 
test <- test %>% filter(date <= floor_date(EVENTS.begin_date, "month") & 
                          date <= floor_date(EVENTS.begin_date, "month") %m+% months(1))

# this yields exactly the same number of obs as before, which i think is not right...

events <- events %>%
  # match unemployment time t
  # if series.id == series.id & time == time
  mutate(unemp = ifelse(events$series.id == unemp_adju$series.id & 
                          floor_date(events$EVENTS.begin_date, unit = "months") == unemp_adju$date, 
                        unemp_adju$unemp, NA),
         # match unemp time t+1
         # if series.id == series.id & time t+1 == time t+1
         unemp_1 = ifelse(events$series.id == unemp_adju$unemp & 
                            floor_date(add_with_rollback(events$EVENTS.begin_date, months(1)), 
                                       unit = "months") == unemp_adju$date, 
                          unemp_adju$unemp, NA),
         # if series.id == series.id & time t+2 == time t+2
         unemp_2 = ifelse(events$series.id == unemp_adju$unemp & 
                            floor_date(add_with_rollback(events$EVENTS.begin_date, months(2)), 
                                       unit = "months") == unemp_adju$date, 
                          unemp_adju$unemp, NA),
         # if series.id == series.id & time t+3 == time t+3
         unemp_3 = ifelse(events$series.id == unemp_adju$unemp & 
                            floor_date(add_with_rollback(events$EVENTS.begin_date, months(3)), 
                                       unit = "months") == unemp_adju$date, 
                          unemp_adju$unemp, NA),
         # if series.id == series.id & time t+4 == time t+4
         unemp_4 = ifelse(events$series.id == unemp_adju$unemp & 
                            floor_date(add_with_rollback(events$EVENTS.begin_date, months(4)), 
                                       unit = "months") == unemp_adju$date, 
                          unemp_adju$unemp, NA),
         # if series.id == series.id & time t+5 == time t+5
         unemp_5 = ifelse(events$series.id == unemp_adju$unemp & 
                            floor_date(add_with_rollback(events$EVENTS.begin_date, months(5)), 
                                       unit = "months") == unemp_adju$date, 
                          unemp_adju$unemp, NA),
         # if series.id == series.id & time t+6 == time t+6
         unemp_6 = ifelse(events$series.id == unemp_adju$unemp & 
                            floor_date(add_with_rollback(events$EVENTS.begin_date, months(6)), 
                                       unit = "months") == unemp_adju$date, 
                          unemp_adju$unemp, NA)
         )

         


#### mutate & ifelse ####

test <- events
test <- test %>%
  # unemp at the time
  mutate(unemp_t = 
           #condition (match on series.id/location, year and month)
           if_else(unemp_adju$series.id == events$series.id & 
                                      unemp_adju$Year == events$Year &
                                      unemp_adju$Month == events$Month, 
                   # TRUE
                   unemp_adju$unemp,
                   # FALSE
                   NA),
         # unemp + 1 month
         # same conditions
         unemp_1 = unemp_adju$series.id == events$series.id & 
           unemp_adju$Year == events$Year &
           unemp_adju$Month == events$Month, 
         # TRUE
         # this is the part which i don't think is possible in this way.
         unemp_adju$unemp + months(1),
         # FALSE
         NA)
