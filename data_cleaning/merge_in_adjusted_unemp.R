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

# merge unemp_adju into events by date, series.
# but ALSO keep unemp up to 6+ months after the event date
test <- full_join(events, unemp_adju, by=c("Year", "Month", "series.id")) 
test <- test %>% filter(date <= floor_date(EVENTS.begin_date, "month") & 
                          date <= floor_date(EVENTS.begin_date, "month") %m+% months(6))


