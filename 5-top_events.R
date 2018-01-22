# francisco
# 5. isolating some relationships
# in this script i try to isolate some big effects so we can see if a relationship is present 
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

# read in  the events data.
events <- read.csv("data/4_sheldus.csv", stringsAsFactors = FALSE)
# read in unemp separately 
unemp <- read.csv("data/bls_vars.csv", stringsAsFactors = FALSE)
# arrange by most populous and biggest damage impact
top_events <- events %>% arrange(-pop_est, -adj.dmg.pcapita) %>%
  head(20)

# find unique counties in top events
top_series <- unique(top_events$series.id)
# filter unemployment to just those counties
top_unemp <- unemp %>% filter(series.id %in% top_series) %>% arrange(date)

top_dates <- top_events %>% select(series.id, EVENTS.begin_date) %>% 
  arrange(EVENTS.begin_date) %>% distinct()

# plot unemployment
##
# not sure why the event lines are not showing up.
ggplot(data = top_unemp, aes(x =  date, y = unemp, group = fcc.county.name))  + geom_line(aes(color = fcc.county.name)) + 
  geom_vline(data = top_dates, aes(xintercept = c(as.numeric(floor_date(ymd(EVENTS.begin_date), unit = "month")))))
       
# doesn't work
cook.events <- top_unemp %>% filter(fcc.county.name=="cook") %>% 
    left_join(filter(top_dates, series.id=="LAUCN170310000000003"), by = c("series.id","EVENTS.begin_date"))
str(top_unemp$date)
top_dates$series.id
str(head(top_dates$EVENTS.begin_date))
