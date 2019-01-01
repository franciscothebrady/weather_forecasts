# francisco 
# what this script does
# 1. read in events.csv
# 2. download regional gdp figures 
# 3. data wrangling 
# 4. merge gdp onto events 

library(tidyverse)
library(bea.R)

# read in 3_events.csv (from 3_historical distribution.R)
events <- read_csv("data/3_events.csv", col_names = TRUE)

# get time range
start_year <- lubridate::year(min(events$EVENTS.begin_date))
end_year   <- lubridate::year(max(events$EVENTS.end_date))

beakey <- "this is where you put your API key from api_keys.txt"
#-- get real GDP by MSA for start_year:end_year
#-- (https://www.bea.gov/API/bea_web_service_api_user_guide.htm)
beaSpecs <- list(
  "UserID" = ,
  "method" = "GetData",
  "datasetname" = "RegionalProduct",
  "Component" = "RGDP_MAN",
  "IndustryId" = "1",
  "GeoFIPS" = "MSA",
  "Year" = as.character(start_year:end_year),
  "ResultFormat" = "json"
  )
gdp_msa <- beaGet(beaSpecs, asWide = FALSE)
rm(beaSpecs, start_year, end_year)

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


# here is where we would read in the events and tack on the gdp. 
events <- read_csv("data/2_events.csv")
