# francisco
# downloading sheldus data
#
# what this file does:
# 1. reads in sheldus data
# 2. cleans and combines them into one file
# 3. deflates sheldus data using 2009 CPI
# 4. reads in events data
# 5. cleans and merges events and sheldus data
# 6. spits out a csv for further processing 

library(readr)
library(lubridate)
#install.packages("quantmod")
library(quantmod)
library(reshape2)
library(tidyr)
library(stringr)

options(scipen = 999) # Do not print scientific notation
# options(stringsAsFactors = FALSE) # Do not load strings as factors

setwd("~/weather_forecasts/")
#### load in sheldus data ####
# all 50 states, [fill this in with the rest of the SHELDUS parameters]
A1 <- read_csv("~/weather_forecasts/data/SHELDUS/UID12986f_AGG_A.csv")
A2 <- read_csv("~/weather_forecasts/data/SHELDUS/UID12986f_AGG_A2.csv")
A3 <- read_csv("~/weather_forecasts/data/SHELDUS/UID12986f_AGG_A3.csv")
A4 <- read_csv("~/weather_forecasts/data/SHELDUS/UID12986f_AGG_A4.csv")
A5 <- read_csv("~/weather_forecasts/data/SHELDUS/UID12986f_AGG_A5.csv")
# combine all the files together
sheldus <- rbind(A1, A2, A3, A4, A5)
names(sheldus)
names(sheldus) <- c("state","county","county.FIPS", "year", "month", "crop.dmg", "adj.09.crop.dmg","crop.09.dmg.pcapita",
                    "prop.dmg","adj.09.prop.dmg","prop.09.dmg.pcapita","inj","inj.pcapita","fatal","fatal.pcapita",
                    "duration.days","fatal.duration","inj.duration","prop.dmg.duration","crop.dmg.duration","records")
sheldus$county.FIPS <- substring(sheldus$county.FIPS, 2, 6)
sheldus$state <- tolower(sheldus$state)
# combine prop and crop values for total damages
sheldus$adj.dmg.tot <- sheldus$adj.09.crop.dmg + sheldus$adj.09.prop.dmg

# we should keep the EVENTS.ID, to merge on later.
anyDuplicated(events$EVENTS.ID)

# use CPI to adjust events data, (2009 base year)
getSymbols("CPIAUCSL", src='FRED') #Consumer Price Index for All Urban Consumers: All Items
avg.cpi <- apply.yearly(CPIAUCSL, mean)

cf <- avg.cpi/as.numeric(avg.cpi['2009'])

# deflate damage values using 2009 CPI
events$adjusted2009damage_value <- matrix(unlist(lapply(events$Year, function(x) {
  as.data.frame(cf$CPIAUCSL[match(x, year(cf[,1]))])[,1]})), 
  byrow=T)[,1] * events$EVENTS.damage_value * 10^events_sheldus$EVENTS.damage_magnitude

#### read in events for merge ####

# join events and sheldus data by Year/Month and Fips/State
events_sheldus <- inner_join(x = events, y = sheldus, by = c("Year"="year", "Month"="month", "fcc.county.FIPS"="county.FIPS",
                                                               "state.name"="state"))
# create total dmg per capita variable by adding crop and prop dmg
events_sheldus$adj.dmg.pcapita <- events_sheldus$crop.09.dmg.pcapita + events_sheldus$prop.09.dmg.pcapita

# sanity tests 
# diff <- as.data.frame(events_sheldus$adj.dmg.tot - 
#                         (events_sheldus$adjusted2009damage_value*10^events_sheldus$EVENTS.damage_magnitude))
# biggest_diff <- events_sheldus[diff == min(diff),]
# summary(diff)

# massage unemployment data for merging
bls_vars <- read_csv("data/bls_vars.csv")

bls_vars$Year <- lubridate::year(bls_vars$date)
bls_vars$Month <- lubridate::month(bls_vars$date)
# merge unemp for current month into events
events_sheldus_unemp <- merge(events_sheldus, bls_vars, by.x = c("Year", "Month", "series.id", "fcc.county.FIPS", "fcc.county.name"), 
                              by.y = c("Year","Month", "series.id", "fcc.county.FIPS", "fcc.county.name"))

names(events_sheldus_unemp)
events <- select(events_sheldus_unemp, Year, Month, EVENTS.ID, EVENTS.begin_date, EVENTS.begin_time_UTC, state, 
                       series.id, fcc.county.FIPS, fcc.county.name, EVENTS.begin_lat, EVENTS.begin_lon, EVENTS.wfo, 
                       adj.dmg.tot, unemp, adj.dmg.pcapita)

county.pop.ests <- read_csv("data/county_pop_ests/PEP_2016_PEPANNRES_with_ann.csv", 
                            skip = 1)
names(county.pop.ests) <- c("geo.id","fips","geography","census.2010","base.2010",
                            "est.2010","est.2011","est.2012","est.2013","est.2014","est.2015","est.2016")
county.pop.ests <- select(county.pop.ests, -base.2010, -est.2010)
county.pop.ests <- separate(county.pop.ests, geography, c("county","state"), sep = ",")
county.pop.ests <- gather(county.pop.ests, year, pop_est, census.2010:est.2016)
county.pop.ests <- mutate(county.pop.ests, year = as.numeric(str_sub(year, -4)))

# merge with events df
events_pop <- left_join(events_final, county.pop.ests, by = c("fcc.county.FIPS"="fips", "Year"="year"))
names(events_pop)[names(events_pop) == "state.x"] <- "state.abb"
names(events_pop)[names(events_pop) == "state.y"] <- "state.name"
events_pop$state.name <- trimws(events_pop$state.name)
# write to data 
write.csv(events_pop, "data/4_sheldus.csv", row.names = FALSE)


# move the below to analysis script
# freq <- events_pop %>% group_by(fcc.county.FIPS, EVENTS.begin_date) %>% 
#   summarise(n()) %>% arrange(desc(pop_est)) 
# 
# events_final %>% arrange(desc(pop_est, adj.dmg.pcapita)) %>% head(15) -> largest_pop_events
# 
# largest_by_date <- largest_pop_events %>%
#   group_by(EVENTS.begin_date, fcc.county.FIPS) %>%
#   summarise(mean.adj.dmg.pcapita = mean(adj.dmg.pcapita)) %>% arrange(desc(pop_est, adj.dmg.pcapita))

# 
##### move to visualization script ####
# unemplot <- ggplot(events_sum, aes(Date, unemp)) +
#   geom_point(aes(colour = factor(fcc.county.FIPS)), 
#              size = 4, show.legend = TRUE, na.rm=TRUE) +
#   geom_vline(data = events_sum, 
#              aes(xintercept = as.numeric(Date[mean.sheldus.dmg==max(mean.sheldus.dmg, na.rm = TRUE)]))) +
#   geom_vline(data = events_sum, 
#              aes(xintercept = as.numeric(Date[mean.adj.dmg.total==max(mean.adj.dmg.total, na.rm = TRUE)])))
# unemplot
# 
