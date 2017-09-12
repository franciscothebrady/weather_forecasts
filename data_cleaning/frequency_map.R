# event frequencies
# combining 15 years of data on storm events to build a frequency measure for severe rain events in CONUS
# timeframe: 2000-2000

setwd("~/weather_forecasts/")
library(stringr)
library(dplyr)
# read in 2000
zero <- as.data.frame(read.csv("data/StormEvents_details-ftp_v1.0_d2000_c20170717.csv.gz", stringsAsFactors = FALSE))
# filter out events outside CONUS, HI, and AK.
zero <- dplyr::filter(zero, STATE_FIPS < 57 & STATE_FIPS > 0)
# filter out episodes with missing damage reports
zero <- dplyr::filter(zero, !(DAMAGE_PROPERTY == "") & !(DAMAGE_CROPS == ""))
#-- filter for PRCP related events=="Heavy Rain"
zero_precip <- dplyr::filter(zero, EVENT_TYPE == "Heavy Rain")
rm(zero)

# read in 2001
one <- as.data.frame(read.csv("data/StormEvents_details-ftp_v1.0_d2001_c20170717.csv.gz", stringsAsFactors = FALSE))
# filter out events outside CONUS, HI, and AK.
one <- dplyr::filter(one, STATE_FIPS < 57 & STATE_FIPS > 0)
# filter out episodes with missing damage reports
one <- dplyr::filter(one, !(DAMAGE_PROPERTY == "") & !(DAMAGE_CROPS == ""))
#-- filter for PRCP related events=="Heavy Rain"
one_precip <- dplyr::filter(one, EVENT_TYPE == "Heavy Rain")
rm(one)

# read in 2002
two <- as.data.frame(read.csv("data/StormEvents_details-ftp_v1.0_d2002_c20170717.csv.gz", stringsAsFactors = FALSE))
# filter out events outside CONUS, HI, and AK.
two <- dplyr::filter(two, STATE_FIPS < 57 & STATE_FIPS > 0)
# filter out episodes with missing damage reports
two <- dplyr::filter(two, !(DAMAGE_PROPERTY == "") & !(DAMAGE_CROPS == ""))
#-- filter for PRCP related events=="Heavy Rain"
two_precip <- dplyr::filter(two, EVENT_TYPE == "Heavy Rain")
rm(two)

# read in 2003
three <- as.data.frame(read.csv("data/StormEvents_details-ftp_v1.0_d2003_c20170717.csv.gz", stringsAsFactors = FALSE))
# filter out events outside CONUS, HI, and AK.
three <- dplyr::filter(three, STATE_FIPS < 57 & STATE_FIPS > 0)
# filter out episodes with missing damage reports
three <- dplyr::filter(three, !(DAMAGE_PROPERTY == "") & !(DAMAGE_CROPS == ""))
#-- filter for PRCP related events=="Heavy Rain"
three_precip <- dplyr::filter(three, EVENT_TYPE == "Heavy Rain")
rm(three)

# read in 2004
four <- as.data.frame(read.csv("data/StormEvents_details-ftp_v1.0_d2004_c20170717.csv.gz", stringsAsFactors = FALSE))
# filter out events outside CONUS, HI, and AK.
four <- dplyr::filter(four, STATE_FIPS < 57 & STATE_FIPS > 0)
# filter out episodes with missing damage reports
four <- dplyr::filter(four, !(DAMAGE_PROPERTY == "") & !(DAMAGE_CROPS == ""))
#-- filter for PRCP related events=="Heavy Rain"
four_precip <- dplyr::filter(four, EVENT_TYPE == "Heavy Rain")
rm(four)

# read in 2005 
five <- as.data.frame(read.csv("data/StormEvents_details-ftp_v1.0_d2005_c20170717.csv.gz", stringsAsFactors = FALSE))
# filter out events outside CONUS, HI, and AK.
five <- dplyr::filter(five, STATE_FIPS < 57 & STATE_FIPS > 0)
# filter out episodes with missing damage reports
five <- dplyr::filter(five, !(DAMAGE_PROPERTY == "") & !(DAMAGE_CROPS == ""))
#-- filter for PRCP related events=="Heavy Rain"
five_precip <- dplyr::filter(five, EVENT_TYPE == "Heavy Rain")
rm(five)

# read in 2006
six <- as.data.frame(read.csv("data/StormEvents_details-ftp_v1.0_d2006_c20170717.csv.gz", stringsAsFactors = FALSE))
# filter out events outside CONUS, HI, and AK.
six <- dplyr::filter(six, STATE_FIPS < 57 & STATE_FIPS > 0)
# filter out episodes with missing damage reports
six <- dplyr::filter(six, !(DAMAGE_PROPERTY == "") & !(DAMAGE_CROPS == ""))
#-- filter for PRCP related events=="Heavy Rain"
six_precip <- dplyr::filter(six, EVENT_TYPE == "Heavy Rain")
rm(six)

# read in 2007
seven <- as.data.frame(read.csv("data/StormEvents_details-ftp_v1.0_d2007_c20170717.csv.gz", stringsAsFactors = FALSE))
# filter out events outside CONUS, HI, and AK.
seven <- dplyr::filter(seven, STATE_FIPS < 57 & STATE_FIPS > 0)
# filter out episodes with missing damage reports
seven <- dplyr::filter(seven, !(DAMAGE_PROPERTY == "") & !(DAMAGE_CROPS == ""))
#-- filter for PRCP related events=="Heavy Rain"
seven_precip <- dplyr::filter(seven, EVENT_TYPE == "Heavy Rain")
rm(seven)

# read in 2008
eight <- as.data.frame(read.csv("data/StormEvents_details-ftp_v1.0_d2008_c20170718.csv.gz", stringsAsFactors = FALSE))
# filter out events outside CONUS, HI, and AK.
eight <- dplyr::filter(eight, STATE_FIPS < 57 & STATE_FIPS > 0)
# filter out episodes with missing damage reports
eight <- dplyr::filter(eight, !(DAMAGE_PROPERTY == "") & !(DAMAGE_CROPS == ""))
#-- filter for PRCP related events=="Heavy Rain"
eight_precip <- dplyr::filter(eight, EVENT_TYPE == "Heavy Rain")
rm(eight)

# read in 2009
nine <- as.data.frame(read.csv("data/StormEvents_details-ftp_v1.0_d2009_c20170816.csv.gz", stringsAsFactors = FALSE))
# filter out events outside CONUS, HI, and AK.
nine <- dplyr::filter(nine, STATE_FIPS < 57 & STATE_FIPS > 0)
# filter out episodes with missing damage reports
nine <- dplyr::filter(nine, !(DAMAGE_PROPERTY == "") & !(DAMAGE_CROPS == ""))
#-- filter for PRCP related events=="Heavy Rain"
nine_precip <- dplyr::filter(nine, EVENT_TYPE == "Heavy Rain")
rm(nine)

# read in 2010
ten <- as.data.frame(read.csv("data/StormEvents_details-ftp_v1.0_d2010_c20170726.csv.gz", stringsAsFactors = FALSE))
# filter out events outside CONUS, HI, and AK.
ten <- dplyr::filter(ten, STATE_FIPS < 57 & STATE_FIPS > 0)
# filter out episodes with missing damage reports
ten <- dplyr::filter(ten, !(DAMAGE_PROPERTY == "") & !(DAMAGE_CROPS == ""))
#-- filter for PRCP related events=="Heavy Rain"
ten_precip <- dplyr::filter(ten, EVENT_TYPE == "Heavy Rain")
rm(ten)

# read in 2011
eleven <- as.data.frame(read.csv("data/StormEvents_details-ftp_v1.0_d2011_c20170519.csv.gz", stringsAsFactors = FALSE))
# filter out events outside CONUS, HI, and AK.
eleven <- dplyr::filter(eleven, STATE_FIPS < 57 & STATE_FIPS > 0)
# filter out episodes with missing damage reports
eleven <- dplyr::filter(eleven, !(DAMAGE_PROPERTY == "") & !(DAMAGE_CROPS == ""))
#-- filter for PRCP related events=="Heavy Rain"
eleven_precip <- dplyr::filter(eleven, EVENT_TYPE == "Heavy Rain")
rm(eleven)

# read in 2012
twelve <- as.data.frame(read.csv("data/StormEvents_details-ftp_v1.0_d2012_c20170519.csv.gz", stringsAsFactors = FALSE))
# filter out events outside CONUS, HI, and AK.
twelve <- dplyr::filter(twelve, STATE_FIPS < 57 & STATE_FIPS > 0)
# filter out episodes with missing damage reports
twelve <- dplyr::filter(twelve, !(DAMAGE_PROPERTY == "") & !(DAMAGE_CROPS == ""))
#-- filter for PRCP related events=="Heavy Rain"
twelve_precip <- dplyr::filter(twelve, EVENT_TYPE == "Heavy Rain")
rm(twelve)

# read in 2013
thirteen <- as.data.frame(read.csv("data/StormEvents_details-ftp_v1.0_d2013_c20170519.csv.gz", stringsAsFactors = FALSE))
# filter out events outside CONUS, HI, and AK.
thirteen <- dplyr::filter(thirteen, STATE_FIPS < 57 & STATE_FIPS > 0)
# filter out episodes with missing damage reports
thirteen <- dplyr::filter(thirteen, !(DAMAGE_PROPERTY == "") & !(DAMAGE_CROPS == ""))
#-- filter for PRCP related events=="Heavy Rain"
thirteen_precip <- dplyr::filter(thirteen, EVENT_TYPE == "Heavy Rain")
rm(thirteen)

# read in 2014
fourteen <- as.data.frame(read.csv("data/StormEvents_details-ftp_v1.0_d2014_c20170718.csv.gz", stringsAsFactors = FALSE))
# filter out events outside CONUS, HI, and AK.
fourteen <- dplyr::filter(fourteen, STATE_FIPS < 57 & STATE_FIPS > 0)
# filter out episodes with missing damage reports
fourteen <- dplyr::filter(fourteen, !(DAMAGE_PROPERTY == "") & !(DAMAGE_CROPS == ""))
#-- filter for PRCP related events=="Heavy Rain"
fourteen_precip <- dplyr::filter(fourteen, EVENT_TYPE == "Heavy Rain")
rm(fourteen)

# read in 2015
fifteen <- as.data.frame(read.csv("data/StormEvents_details-ftp_v1.0_d2015_c20170718.csv.gz", stringsAsFactors = FALSE))
# filter out events outside CONUS, HI, and AK.
fifteen <- dplyr::filter(fifteen, STATE_FIPS < 57 & STATE_FIPS > 0)
# filter out episodes with missing damage reports
fifteen <- dplyr::filter(fifteen, !(DAMAGE_PROPERTY == "") & !(DAMAGE_CROPS == ""))
#-- filter for PRCP related events=="Heavy Rain"
fifteen_precip <- dplyr::filter(fifteen, EVENT_TYPE == "Heavy Rain")
rm(fifteen)

# combine into one big old df
freq_totals <- rbind(zero_precip, one_precip, two_precip, three_precip, four_precip, five_precip, six_precip, seven_precip, eight_precip,
                     nine_precip, ten_precip, eleven_precip, twelve_precip, thirteen_precip, fourteen_precip, fifteen_precip)
rm(zero_precip, one_precip, two_precip, three_precip, four_precip, five_precip, six_precip, seven_precip, eight_precip,
   nine_precip, ten_precip, eleven_precip, twelve_precip, thirteen_precip, fourteen_precip, fifteen_precip)
# remove unnecessary cols
freq_totals <- select(freq_totals, "STATE","STATE_FIPS","YEAR","EVENT_TYPE","CZ_TYPE","CZ_FIPS",
                      "CZ_NAME","WFO","BEGIN_DATE_TIME","CZ_TIMEZONE","END_DATE_TIME","INJURIES_DIRECT",
                      "INJURIES_INDIRECT","DEATHS_DIRECT","DEATHS_INDIRECT","DAMAGE_PROPERTY","DAMAGE_CROPS",
                      "BEGIN_LAT","BEGIN_LON","END_LAT","END_LON")

# recode crop and property damage vars
damage_magnitude <- cbind(
  strsplit(substr(freq_totals$DAMAGE_PROPERTY, nchar(freq_totals$DAMAGE_PROPERTY), nchar(freq_totals$DAMAGE_PROPERTY)), ""),
  strsplit(substr(freq_totals$DAMAGE_CROPS, nchar(freq_totals$DAMAGE_CROPS), nchar(freq_totals$DAMAGE_CROPS)), ""))
damage_magnitude <- ifelse(damage_magnitude == "K", 3, ifelse(damage_magnitude == "M", 6, 9))
damage_numeric <- cbind(
  as.numeric(strsplit(freq_totals$DAMAGE_PROPERTY, "[[:alpha:]]")),
  as.numeric(strsplit(freq_totals$DAMAGE_CROPS, "[[:alpha:]]")))
damage_value <- rowSums(damage_numeric * 10^damage_magnitude, na.rm = TRUE)
freq_totals$DAMAGE_VALUE <- damage_value / 1e3
freq_totals$DAMAGE_VALUE.magnitude <- rep(3, length(freq_totals$DAMAGE_VALUE))
freq_totals$DAMAGE_VALUE.unit <- rep("USD", length(freq_totals$DAMAGE_VALUE))
rm(damage_magnitude, damage_numeric, damage_value)

freq_totals$DAMAGE_VALUE <- freq_totals$DAMAGE_VALUE*10^freq_totals$DAMAGE_VALUE.magnitude
freq_totals$DAMAGE_CROPS <- NULL
freq_totals$DAMAGE_PROPERTY <- NULL
freq_totals$DAMAGE_VALUE.magnitude <- NULL

# combine injuries/fatalities, could be interesting later
freq_totals$injuries.deaths <- freq_totals$INJURIES_DIRECT + freq_totals$INJURIES_INDIRECT +
  freq_totals$DEATHS_DIRECT + freq_totals$DEATHS_INDIRECT
freq_totals$INJURIES_DIRECT <- NULL
freq_totals$INJURIES_INDIRECT <- NULL
freq_totals$DEATHS_DIRECT <- NULL
freq_totals$DEATHS_INDIRECT <- NULL

# group by NWS Region
wfo_reg <- read.csv(url("https://raw.githubusercontent.com/franciscothebrady/weather_forecasts/master/WFO-freq.csv"), 
                    stringsAsFactors = FALSE)
# remove Alaska becuase it's useless!
wfo_reg <- filter(wfo_reg, region!="Alaska")
# match WFOs in freq totals 
freq_totals <- merge(freq_totals, wfo_reg, by.x = c("WFO","YEAR"), by.y = c("wfo","Year"), all.x = TRUE)
rm(wfo_reg)
# MAPS!
# group by state (this gives us the total in the last 15 years by state)
state_freq <- group_by(freq_totals, STATE)
# which we can then summarize!
state_freq <- count(state_freq)

# install.packages("choroplethr")
# install.packages("choroplethrMaps")
library(choroplethr)
library(choroplethrMaps)
# format so the state names match up
state_freq$STATE <- tolower(state_freq$STATE)
names(state_freq) <- c("region","value")
state_choropleth(state_freq, title = "Historical Severe Precipitation, 2000-2015") # warning: creates large file.
#rm(historical_freq)

# dfGroup <- energy %>% group_by(datetime) %>% summarise(value)
