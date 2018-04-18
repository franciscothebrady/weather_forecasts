# visualizations
# try to get some cool viz with this data
library(ggplot2)
library(dplyr)

# read in events data and seasonally adjusted unemployment
events <- read.csv("data/6_events_processed.csv", stringsAsFactors = FALSE)

unemp_adju <- read.csv("data/unemp_adju.csv", stringsAsFactors = FALSE)

# aim: grab unemployment for each location
#      from: t0 - time of the event 
#      to:   t6 - time 6 months after event
# 
# 1. read in events/ unemp
# 2. merge by event start date
# 3. ?????
events$unemp <- NULL

# create df of unemp and events
new_df <- left_join(events, unemp_adju, by = "series.id") 
new_df <- filter(new_df, as.Date(new_df$date) >= as.Date(new_df$EVENTS.begin_date))
new_df$EVENTS.begin_date <- ymd(new_df$EVENTS.begin_date)
new_df$date <- ymd(new_df$date)


ggplot(new_df, aes(date, unemp, group=fcc.county.FIPS, color=unemp)) + 
  geom_line() +
  theme(axis.text.x = element_text(angle = 90)) + 
  geom_vline(data = new_df, aes(xintercept = as.numeric(EVENTS.begin_date),
                                size=adj.dmg.pcapita)) 
