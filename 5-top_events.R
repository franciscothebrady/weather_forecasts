# francisco
# 5. isolating some relationships
# in this script i try to isolate some big effects so we can see if a relationship is present 
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

# read in  the events data.
events <- read.csv("data/4_sheldus.csv", stringsAsFactors = FALSE)
# change begin_date to date class
events$EVENTS.begin_date <- as.Date(events$EVENTS.begin_date)
# read in unemp separately 
unemp <- read.csv("data/bls_vars.csv", stringsAsFactors = FALSE)
# change dates to date class
unemp$date <- as.Date(unemp$date)
# arrange by most populous and biggest damage impact
top_events <- events %>% arrange(-pop_est, -adj.dmg.pcapita) %>%
  head(75)

# find unique counties in top events
top_series <- unique(top_events$series.id)
# filter unemployment to just those counties
top_unemp <- unemp %>% filter(series.id %in% top_series) %>% arrange(date)

top_dmg <- top_events %>% filter(series.id %in% top_series) %>% 
  select(EVENTS.begin_date, fcc.county.name, fcc.county.FIPS, adj.dmg.pcapita, series.id)

allthestuff <- merge(top_unemp, top_dmg) # merged all together

top_dates <- top_events %>% select(series.id, fcc.county.name, EVENTS.begin_date) %>% 
  arrange(EVENTS.begin_date) %>% distinct() %>% mutate(date = floor_date(ymd(EVENTS.begin_date), unit = "month"))

# plot unemployment and damage per capita 
# employment
unemplot <- ggplot(data = allthestuff, aes(x =  date, y = unemp, group = fcc.county.name)) + 
  geom_line(aes(color = fcc.county.name)) + guides("FALSE") +
  geom_point(data = allthestuff, aes(x = EVENTS.begin_date, y = adj.dmg.pcapita,
                                 group = fcc.county.name, color = fcc.county.name, size = adj.dmg.pcapita)) +
  geom_vline(data = allthestuff, aes(xintercept = as.numeric(EVENTS.begin_date),
                                     color = factor(fcc.county.name)), show.legend = FALSE) +
  scale_x_date(date_breaks = waiver(), date_labels = "%Y-%b")

unemplot

smaller_sample <- filter(allthestuff, fcc.county.name==c("clark","maricopa","riverside"))
small_plot <- ggplot(data = smaller_sample, aes(x =  date, y = unemp, group = fcc.county.name)) + 
  geom_line(aes(color = fcc.county.name)) + guides("FALSE") +
  geom_point(data = smaller_sample, aes(x = EVENTS.begin_date, y = adj.dmg.pcapita,
                                     group = fcc.county.name, color = fcc.county.name, size = adj.dmg.pcapita)) +
  geom_vline(data = smaller_sample, aes(xintercept = as.numeric(EVENTS.begin_date),
                                     color = factor(fcc.county.name)), show.legend = FALSE) +
  scale_x_date(date_breaks = "3 months", date_labels = "%m-%y") + theme(axis.text.x = element_text(angle = -45, hjust = 0.5))
small_plot
# plot for event lines 
# not sure why the event lines are not showing up.
# ggplot(data = top_unemp, aes(x =  date, y = unemp, group = fcc.county.name))  + geom_line(aes(color = fcc.county.name)) + 
#   geom_vline(data = top_dates, aes(xintercept = c(as.numeric(floor_date(ymd(EVENTS.begin_date), unit = "month")))))

# ggplot(data = top_unemp, aes(x =  date, y = unemp, group = fcc.county.name)) + geom_line(aes(color = fcc.county.name)) +
#   geom_point(data = top_dmg, aes(x = EVENTS.begin_date, y = adj.dmg.pcapita, 
#                                  group = fcc.county.name, color = fcc.county.name, size = adj.dmg.pcapita)) + 
#   facet_grid(type ~., scales = "free") #sec.axis = sec_axis(~.*0.001+0.005, name = "damage per capita")) 


# doesn't work
# cook.events <- top_unemp %>% filter(fcc.county.name=="cook") %>% 
#     left_join(filter(top_dates, series.id=="LAUCN170310000000003"), by = c("series.id","EVENTS.begin_date"))
# str(top_unemp$date)
# top_dates$series.id
# str(head(top_dates$EVENTS.begin_date))

# plot dates on top of unemployment
# event_plot <- u + geom_vline(mapping = top_dates)
