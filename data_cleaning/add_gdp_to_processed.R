# francisco
# weather forecast
# adding gdp and census block codes
# for 2014-15, add 2010-2013 later
# plan:
# 1. grab gdp/msa for years
# 2. merge gdp onto obs
# 3. grab census block id for obs
# 4. cbind onto obs
# 5. ????
# 6. profit!

# load libraries
install.packages("dplyr")
library(dtplyr)
library(bea.R)

# load processed 2015 and 2014 storm events, obs, and forecasts

fteen <- read.csv("data/2015_storm_events_processed.csv", stringsAsFactors = FALSE)
frteen <- read.csv("data/2014_storm_events_processed.csv", stringsAsFactors = FALSE)

# remove extra columns (this should be done by name instead of index)
fteen <- fteen[,-1]
frteen <- frteen[,-c(1,37,38)]

combined_events <- rbind(fteen, frteen)

# using the FCC API to match lat/lon to census tracts
# census block conversion API docs here: https://www.fcc.gov/general/census-block-conversions-api

# install.packages("httr")
# install.packages("jsonlite")
library(jsonlite)
library(httr)
options(stringsAsFactors = FALSE)

# following this as an example: http://tophcito.blogspot.com/2015/11/accessing-apis-from-r-and-little-r.html#fn2
# set up the url and parameters

url <- "http://data.fcc.gov/api/block/find?format=json"


latitude <- combined_events$EVENTS.begin_lat

longitude <- combined_events$EVENTS.begin_lon

request <- paste0(url, "&latitude=", latitude, "&longitude=", longitude, "&showall=false")

str(request)

# change to whatever number of obs we end up having (2014-15 = 1167)
tracts <- data.frame(FIPS = rep(0, 1167),
                     County.FIPS = rep(0, 1167),
                     County.name = rep(0, 1167),
                     State.FIPS = rep(0, 1167),
                     State.code = rep(0, 1167),
                     State.name = rep(0, 1167),
                     status     = rep(0, 1167),
                     executionTime = rep(0, 1167))

nevents <- length(combined_events$EVENTS.begin_lat)
#  something about the way this request works requires the df setup beforehand so it fills in as.data.frame nicely.
for (i in 1:nevents) {
  latitude <- combined_events$EVENTS.begin_lat[i]
  longitude <- combined_events$EVENTS.begin_lon[i]
  request <- fromJSON(paste0(url, "&latitude=", latitude, "&longitude=", longitude, "&showall=false"))
  tracts[i,] <- as.data.frame.list(request)
}

tracts

# cbind with combined_events

combined_events <- cbind(combined_events, tracts)

# should probably remove some extraneous rows.
# need to set a checkpoint to use the bea.R package for gdp/msa


