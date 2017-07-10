# francisco
# past event frequencies by MSA - using events data from 2010-2015
setwd("C:/Users/franc/OneDrive/Documents/Research/Weather Forecasts/data")
# install.packages("RCurl")
# install.packages("gdata")
library(RCurl)
library(gdata)
library(XML)
library(reshape2)
library(tidyr)
# cbsa defs from census
# NOTE: uploaded to github and read from raw

x <- getURL("https://raw.githubusercontent.com/franciscothebrady/weather_forecasts/master/data/county_msa_list.txt")
cbsa <- read.csv(text = x, stringsAsFactors = FALSE, header = FALSE)
names(cbsa) <- c("CBSA.code", "CBSA.title", "FIPS.state-county", "County.name")

# split county and state names
temp <- colsplit(cbsa$County.name, ", ", names = c("County.name", "State.name"))
cbsa <- cbind(cbsa, temp)

# get rid of county&state col
cbsa <- cbsa[,-4]

# read in storm events (from brian's get_n_process_data.R)
#-- get severe weather data with damage reports from NOAA
#-- (https://www.ncdc.noaa.gov/swdi/#Intro)

# take storm events data for 2010-15
ten<- as.data.frame(read.csv("StormEvents_details-ftp_v1.0_d2010_c20160223.csv.gz", stringsAsFactors = F))
eleven<- as.data.frame(read.csv("StormEvents_details-ftp_v1.0_d2011_c20160223.csv.gz", stringsAsFactors = F))
twelve<- as.data.frame(read.csv("StormEvents_details-ftp_v1.0_d2012_c20160223.csv.gz", stringsAsFactors = F))
thirteen<- as.data.frame(read.csv("StormEvents_details-ftp_v1.0_d2013_c20160223.csv.gz", stringsAsFactors = F))
fourteen<- as.data.frame(read.csv("StormEvents_details-ftp_v1.0_d2014_c20160617.csv.gz", stringsAsFactors = F))
fifteen<- as.data.frame(read.csv("StormEvents_details-ftp_v1.0_d2015_c20160921.csv.gz", stringsAsFactors = F))
# take a look at what got read in
storm_events <- rbind(ten, eleven, twelve, thirteen, fourteen, fifteen)
rm(ten, eleven, twelve, thirteen, fourteen, fifteen)


# filter out events outside CONUS, HI, and AK.
storm_events <- dplyr::filter(storm_events, STATE_FIPS < 57 & STATE_FIPS > 0)

# filter out episodes with missing damage reports
storm_events <- dplyr::filter(storm_events, !(DAMAGE_PROPERTY == "") & !(DAMAGE_CROPS == ""))

# inner join by county and state names 

# function to change abbreviations to full state names, from: https://gist.github.com/ligyxy/acc1410041fe2938a2f5
abb2state <- function(name, convert = F, strict = F){
  data(state)
  # state data doesn't include DC
  state = list()
  state[['name']] = c(state.name,"District Of Columbia")
  state[['abb']] = c(state.abb,"DC")
  
  if(convert) state[c(1,2)] = state[c(2,1)]
  
  single.a2s <- function(s){
    if(strict){
      is.in = tolower(state[['abb']]) %in% tolower(s)
      ifelse(any(is.in), state[['name']][is.in], NA)
    }else{
      # To check if input is in state full name or abb
      is.in = rapply(state, function(x) tolower(x) %in% tolower(s), how="list")
      state[['name']][is.in[[ifelse(any(is.in[['name']]), 'name', 'abb')]]]
    }
  }
  sapply(name, single.a2s)
}


# take state names from events and turn into abbrevs
storm_events$STATE.ABB <- abb2state(storm_events$STATE, convert = T)
# do the same to add state names to CBSA df
cbsa$State.full <- abb2state(cbsa$State.name, convert = F)

# getting a weird encoding error when using tolower on county name. changing all to UTF-8
cbsa$County.name <- stringi::stri_trans_general(cbsa$County.name, "latin-ascii")

# convert cbsa$state.full and abbrev tolower
cbsa$State.full <- tolower(cbsa$State.full)
cbsa$County.name <- tolower(cbsa$County.name)

# convert storm_event$cz_name and state and state abb tolower
storm_events$CZ_NAME <- tolower(storm_events$CZ_NAME)
storm_events$STATE <- tolower(storm_events$STATE)
storm_events$STATE.ABB <- tolower(storm_events$STATE.ABB)
library(dplyr)

# join on names, abbs and county names into storm_events
storm_events <- inner_join(storm_events, cbsa, by = c("STATE" = "State.full", "CZ_NAME" = "County.name", "STATE.ABB" = "State.name"))

# drop the columns we don't need for this analysis
storm_event_freq <- select(storm_events, STATE, STATE_FIPS, STATE.ABB, YEAR,
                           EVENT_TYPE, CBSA.code, CBSA.title, CZ_NAME, "FIPS.state-county")

# filter by precip events (cribbed from brians code again)
storm_events_precip <- dplyr::filter(storm_event_freq,
                                     EVENT_TYPE == "Flash Flood" | 
                                       EVENT_TYPE == "Flood" |
                                       EVENT_TYPE == "Heavy Rain" |
                                       EVENT_TYPE == "Debris Flow")

summary(storm_events_precip)
# not super helpful

# grouping events by MSA
by.MSA <- group_by(storm_events_precip, STATE, EVENT_TYPE, CBSA.code, CBSA.title)
MSA.count <- dplyr::summarize(by.MSA, count=n())
MSA.count

# grouping event totals in each MSA
library(dplyr)
msa.totals <- tally(group_by(storm_events_precip, CBSA.code))


# from brian's example
# install.packages("rgdal")
library(rgdal)
# install.packages("rgeos")
library(rgeos)
library(maptools)
library(ggplot2)
library(ggmap)

# set system local because a few characters in shapefile cause trouble
# when filtering out select US territories from being mapped
Sys.setlocale('LC_ALL', 'C')
setwd("C:/Users/franc/OneDrive/Documents/Research/Weather Forecasts/data")

# read shapefile

## couldnt get this to work for me
sf_cbsa <- "shapefile/tl_2010_us_cbsa10.shp"
cbsa_map <- readOGR(sf_cbsa, layer = "tl_2010_us_cbsa10")

cbsa_map$NAME10

sf_state <- "shapefile/tl_2010_us_state10.shp"
state_map <- readOGR(sf_state, layer = "tl_2010_us_state10")
names(state_map)


# # remove CBSAs, etc. of Alaska, Hawaii, and Puerto Rico
cbsa_map <- cbsa_map[!grepl("AK$|HI$|PR$", cbsa_map$NAME10), ]
length(cbsa_map)   # result should be 933 after remove those three areas
state_map <- state_map[!grepl("Alaska|Hawaii|Puerto", state_map$NAME10), ]
length(state_map)  # result should be 49 after removing those three areas

# make data ggplot friendly
cbsa_map_f <- fortify(cbsa_map, region = "GEOID10")
# ggplot(cbsa_map_f)

state_map_f <- fortify(state_map, region = "GEOID10")

# create map, draw object you want in the background first,
# i.e., state boundaries then CBSA boundaries
bdry_map <- ggplot(NULL) +
  geom_polygon(data = state_map_f, aes(long, lat, group = group), color = "black", fill = "white") +
  geom_polygon(data = cbsa_map_f, aes(long, lat, group = group, color = "blue")) +
  theme_bw()

# output map
bdry_map

# merge event freq by msa data with cbsa map.
cbsa_and_events <- merge(cbsa_map_f, msa.totals, by.x ="id", by.y="CBSA.code")
names(cbsa_and_events)
cbsa_and_events <- rename(cbsa_and_events, event_freq = n)
names(cbsa_and_events)

# add event freq to bdry_map
freq_map <- bdry_map 
