# francisco 
# 3 - add economic variables
# WHAT THIS PROGRAM DOES:
# 1. reads in all the storm event files from 2010-2016
# 2. grabs GDP by MSA for the years 2010-2016
# 3. relates each county in the events data with an MSA
# 4. merges GDP by MSA onto counties within those MSAs
# 5. using MABLE/GEOCORR, decomposes the contribution to GDP of each component county

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

# read in 2_fcc

#   -- get real GDP by MSA for 2010-2016
#   -- (https://www.bea.gov/API/bea_web_service_api_user_guide.htm)   check docs for YEAR
  beaSpecs <- list(
    "UserID" = beakey,
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