# francisco 
# read in events and merge with unemployment

library(tidyverse)
library(lubridate)

# read in events 
events <- read_csv("data/6_events_processed.csv")
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
test <- full_join(events, unemp_adju, by=c("Year", "Month", "series.id")) 
test <- test %>% filter(date <= floor_date(EVENTS.begin_date, "month") & 
                          date <= floor_date(EVENTS.begin_date, "month") %m+% months(6))

# this yields exactly the same number of obs as before, which i think is not right...

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
