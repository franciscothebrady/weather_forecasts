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
# install.packages("dplyr")
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
library(tidyr)

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

#-- get real GDP by MSA for 2014-15
beakey <- "AF498701-0543-490E-B9B3-B850D6166872"



#-- (https://www.bea.gov/API/bea_web_service_api_user_guide.htm)
beaSpecs <- list(
  "UserID" = beakey,
  "method" = "GetData",
  "datasetname" = "RegionalProduct",
  "Component" = "RGDP_MAN",
  "IndustryId" = "1",
  "GeoFIPS" = "MSA",
  "Year" = "2014, 2015",
  "ResultFormat" = "json"
)
gdp_msa <- beaGet(beaSpecs, asWide = FALSE)
rm(beaSpecs)


#-- get CBSA code, area, and pop. den. directly from Census.gov
#-- (https://www.census.gov/programs-surveys/popest.html)
#-- (https://www.census.gov/population/metro/data/pop_data.html)
#-- CBSA = MSA + uSA, "u" is micro

# Modified and coverted
# https://www.census.gov/population/metro/files/CBSA%20Report%20Chapter%203%20Data.xls
# to csv as cbsa_info_2010.csv.
cbsa_info <- read.csv("data/cbsa_info_2010.csv", stringsAsFactors = FALSE)

#-- add CBSA codes to corresponding city/state in gdp_msa

# Edit gdp_msa$GeoName to match msa_code_list$CBSA.Title
gdp_msa$GeoName <- gsub(" (Metropolitan Statistical Area)", "", gdp_msa$GeoName, fixed = TRUE)

# Find correct MSA code for corresponding GeoName by matching msa_code_list$CBSA.Title
# with gdp_msa$GeoName and merge result into gdp_msa
gdp_msa$GeoFips <- as.numeric(gdp_msa$GeoFips)
gdp_msa <- merge(gdp_msa, cbsa_info, by.x = "GeoFips", by.y = "CBSA.code")

# Tidy gdp_msa
gdp_msa <- gdp_msa[, c(4,1,3,11,7,6,5)]  # rearrange columns
colnames(gdp_msa) <- c("YEAR", # 4
                       "CBSA.code", # 1
                       "CBSA.title", # 3
                       "CBSA.pop_density", # 11
                       "MSA.GDP", # 7
                       "MSA.GPD.magnitude", # 6
                       "MSA.GPD.unit") # 5

# We no longer need msa_code_list so remove it from workspace to save RAM.
rm(cbsa_info)

# list of all counties in each msa, from bea: https://www.bea.gov/regional/docs/msalist.cfm
# download as csvhg
msa_list <- read.csv("data_cleaning/metrolist.csv", header = F, sep = ",", fill = T, stringsAsFactors = F, strip.white = TRUE)
?read.csv
names(msa_list) <- c("cbsa.code","msa.name","fips.st.cnty","cnty")

# separate state abbrevs

msa_list <- separate(data = msa_list, col = cnty, into = c("cnty", "st"), sep = ",")
# this worked but:
# Warning messages:
#   1: Too many values at 4 locations: 884, 1012, 1103, 1104 
#   2: Too few values at 3 locations: 1148, 1149, 1150 
# check out the warnings
msa_list[884,] # VA
msa_list[1012,] # VA
msa_list[1103,] # VA
msa_list[1104,] # man wtf VA
msa_list[1148:1150,] # BEA attribution (delete)
# delete BEA attribution
msa_list <- msa_list[-c(1148:1150),]
# reassign all the weird VA names to VA
msa_list$st[884] <- "VA"
msa_list$st[1012] <- "VA"
msa_list$st[1103] <- "VA"
msa_list$st[1104] <- "VA"

### TODO
## Merge MSA county list with GDP list



