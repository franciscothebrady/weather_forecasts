# francisco 
# 3 - add economic variables
# WHAT THIS PROGRAM DOES:
# 1. reads in all the storm event files from 2010-2016
# 2. merge in some economic data that is relevant and at a decent frequency
# 3. ???
# this is the Local Area Unemployment Statistics Series decoder page: https://www.bls.gov/help/hlpforma.htm#LA
# here is the table of contents for BLS statistics: https://www.bls.gov/help/hlpforma.htm#OEUS
  
setwd("~/weather_forecasts/")

#  load libraries
library(dplyr)
library(readr)
library(lubridate)
library(reshape2)
library(stringr)
library(tidyr)
# read in 2_fcc_api.csv
events <- read.csv("data/2_fcc_api.csv", header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# Encoding(events$state.name) still says "unknown", not sure why :/


# read in BLS series ids for all counties
# we need to do some text cleaning, then we should be able to get a df of all the ids and counties
# then we can narrow down to only the counties and ids we need
# then we can pipe those into the BLS api to get the unemployment statistics
cols_fwf <- readr::fwf_empty("data/bls_series_ids.txt")
series.ids <- read_fwf("data/bls_series_ids.txt", col_positions = cols_fwf)
#View(series.ids)
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

# how can we merge these? 
events$fcc.county.FIPS <- sprintf("%05d", events$fcc.county.FIPS)

# do some string massaging
series.ids$county <- gsub("\\bCounty\\b", "", series.ids$county)
series.ids$county <- trimws(series.ids$county, which = "both")
series.ids$state <- trimws(series.ids$state, which = "both")
events$fcc.county.name <- trimws(events$fcc.county.name, which = "both")
events$state.name <- tolower(events$state.name)
events$EVENTS.state <- tolower(events$EVENTS.state)
events$fcc.county.name <- tolower(events$fcc.county.name)
events$EVENTS.czname <- tolower(events$EVENTS.czname)
events$state.code <- tolower(events$state.code)
series.ids$county <- tolower(series.ids$county)
series.ids$state <- tolower(series.ids$state)

# select only series.id counties in events dataset to build request
series.ids$fcc.county.FIPS <- substring(series.ids$series.id, 6, 10)

# merge on fcc.county fips
events <- merge(events, series.ids, by = "fcc.county.FIPS")

# use blsAPI, from: https://www.bls.gov/developers/api_r.htm
library(devtools)
#install_github("mikeasilva/blsAPI")
library(blsAPI)

response_df <- NULL
count <- 1
counties <- unique(events$series.id)
counties <- split(counties, rep(1:3, 475))

# payload creates a list of requests and parameters
for (i in 1:length(counties[[i]][j])) {
  payload <- list(
    'seriesid'=c(counties[[i]][j]), #series.ids$series.id[1:1000]
    'startyear'=2010,
    'endyear'=2016,
    'catalog'=FALSE,
    'registrationKey'='92882bc0dd354a94b65f8b8a232a0b42')
  
  # which is passed to the blsAPI function, 2 means version 2 of the API (requires key), TRUE means return df
  response <- blsAPI(payload, 2, TRUE)
  response_df <- rbind(response_df, response)
  
  count <- count + 1

  if (count==length(counties[[i]])) {
    break
    print("you have reached the end of this list, please continue from another IP address")
      readline("press any key to continue")
      count <- 1
    }
}


# write the whole thing to csv
print("writing to 'county_unemp.csv'")
write.csv(response_df, "data/county_unemp.csv", row.names = FALSE)

# clean up response df
names(response_df) <- c("yyyy","mm","month","unemp","seriesID")
response_df$mm <- gsub("[A-Z]","",response_df$mm)
# full date
response_df$date <- lubridate::ymd(paste0(response_df$yyyy,"-",response_df$mm,"-","01"))

# create a date vector of unique dates
event_dates <- unique(events$EVENTS.begin_date)

#TODO
# filter by month and year for unemp observations we want. 
# merge with events df
# write to csv for the next step!
print("writing to .csv")
write.csv(events, "data/bls_vars.csv", row.names = FALSE)
