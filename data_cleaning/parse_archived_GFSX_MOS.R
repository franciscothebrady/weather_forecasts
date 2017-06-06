#
# simple example how you might use the get_archive_GFSX_MOS function
# and go about parsing the output
#                                           -Brian
#

rm(list = ls())

library(plyr)
library(dplyr)
library(tidyr)
# install.packages("stringr")
library(stringr)
library(readr)
# load function
source("C:/Users/franc/OneDrive/Documents/Research/Weather Forecasts/Script/get_archived_GFSX_MOS.R")

station_id <- "KDCA"
runtime_dt <- "20151223"
runtime_hr <- "00Z"

mos_output <- get_archived_GFSX_MOS(station_id, runtime_dt, runtime_hr)
mos_output

# set start and end vectors (NOTE: we are ignoring CLIMO columns)
start <- seq(1, 32) + rep(seq(0, 30, 2), each = 2)
end   <- seq(1, 32) + c(0, rep(seq(2, 30, 2), each = 2), 32)
# parse each string in mos_output
x1 <- lapply(mos_output[c(2, 4:length(mos_output))], function(s){ str_sub(s, start = start, end = end) })
# drop unwanted elements
y1 <- lapply(x1, function(s){ s[seq(2, 32, 2)] })
# convert list to data frame
z1 <- ldply(y1)
# assign data frame column names (forecast hours)
colnames(z1) <- y1[[1]]
# assign data frame row names (forecast elements)
rownames(z1) <- z1[,1]
# remove first column and row that contains column and row names
mos_df <- z1[-1,-1]
# final version of data frame we can use to do stuff
mos_df

# transpose data frame
mos_df1 <- data.frame(t(mos_df), stringsAsFactors = FALSE)
# append runtime
mos_df1 <- cbind(RTDT=rep(runtime_dt, 15), RTHR=rep(runtime_hr, 15), mos_df1)
# append station id
mos_df1 <- cbind(ICAO=rep(station_id, 15), mos_df1)

mos_df1

# TODO:
#   * change FHR to actual time/date from runtime. E.g., if runtime is 2015/01/01 00Z, then
#     FHR 36 should be 2015/01/02 12Z
# 1. create date col from RTDT
library(lubridate)
# remove Zs, but remember we're still in UTC

###########
###########
mos_df1$RTHR <- gsub(".$", "",mos_df1$RTHR) 

# turn fhr into column
mos_df1$FHR <- rownames(mos_df1)

# use fhr and add to runtime date to get the date for which the forecast is valid
mos_df1$FHR <- ymd_hms(paste(mos_df1$RTDT, "00:00:00"), tz="UTC") + hours(mos_df1$FHR)
mos_df1

#   * fill blanks with NAs
for (i in 4:6){
  mos_df1[,i] <- as.numeric(mos_df1[,i])
}
for (i in 8:17){
  mos_df1[,i] <- as.numeric(mos_df1[,i])
}
mos_df1[,19] <- as.numeric(mos_df1[,19])

mos_df1

