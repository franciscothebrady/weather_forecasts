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
library(maps)
library(plyr)
library(dplyr)
library(dtplyr)
library(stringr)
library(purrr)
library(rnoaa)
library(bea.R)
library(geosphere)
library(weathermetrics)
library(jsonlite)
library(httr)
library(lubridate)

# read in 2_fcc.api.csv
events <- read.csv("data/2_fcc.api.csv", header = TRUE)


#   -- get real GDP by MSA for 2010-2016
#   -- (https://www.bea.gov/API/bea_web_service_api_user_guide.htm)   check docs for YEAR
  beaSpecs <- list(
    "UserID" = # ,
    "method" = "GetData",
    "datasetname" = "RegionalProduct",
    "Component" = "RGDP_MAN",
    "IndustryId" = "1",
    "GeoFIPS" = "MSA",
    "Year" = "2010,2011,2012,2013,2014,2015,2016",   
    "ResultFormat" = "json"                          
  )
  gdp_msa <- beaGet(beaSpecs, asWide = FALSE)
  rm(beaSpecs)

# read in counties in each MSA list
  
# merge in MABLE data
  
# think about other economic variables to include. 
# unemployment?
# jobs claims?
# monthly something?
  