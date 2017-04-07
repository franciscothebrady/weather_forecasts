# bls api test
# install.packages("blsAPI")
# there's another package called blscrapeR: https://github.com/keberwein/blscrapeR
# find out which one works better.
library(rjson)
library(blsAPI)
library(data.table)
library(dplyr)
library(tidyr)
## Accessing unemployment reports for MSAs using the API created by the BLS
# set wd 
setwd("C:/Users/franc/OneDrive/Documents/Research/Weather Forecasts")

# reading in craxy text file so that each line is a "row" in list.
# read in table with unemp for all MSAs, monthly since 1990 (big file)
# we're using this to get the table names all in one column
msa.unemp <- fread("https://www.bls.gov/web/metro/ssamatab1.txt", sep = "?", skip = 4, header = FALSE, verbose = F)
# so it read in all in one column, so we strip the white space and replace with a tab
names(msa.unemp) <- "line"
# parse by replacing all variable spacing w 1 tab. 
msa.unemp$line <- gsub('\\s\\s+', '\t', msa.unemp$line)
# write as new text file w normal separation
writeLines(msa.unemp$line, "msa_unemp.txt")


# read in the new normal df!
msa.unemp.df <- fread("msa_unemp.txt", header = FALSE, stringsAsFactors = FALSE)
# name the vars
names(msa.unemp.df) <- c("laus.code", "state.fips", "area.fips","area.name",
                         "year","month","civ.lf","emp","unemp","unemp.rate")

# this table has 1990-2017, but we only need 2015
unemp.year <- dplyr::filter(msa.unemp.df, year==2015)

unemp.year$unemp <- gsub(",", "", unemp.year$unemp)
unemp.year$unemp <- as.numeric(unemp.year$unemp)

# convert data to long format
# wasn't sure if this would be useful yet. but good to have it here as a reminder of how to use melt
# unemp.year.long <- melt(unemp.year, id.vars = c("laus.code", "state.fips", "area.fips","area.name",
#                                                 "year","month"), variable.name = "measure", 
#                         value.name = "value")

str(unemp.year)
unemp.year$area.fips <- as.factor(unemp.year$area.fips)
unemp.year$month <- as.factor(unemp.year$month)

unemp.year.w.diff <- unemp.year %>% group_by(area.fips) %>%
  mutate(month.diff = c(NA, diff(unemp)))

# calculate monthly changes (should I multiply by 100 because the values are small?)
unemp.year.w.diff$monthly.change <- unemp.year.w.diff$month.diff / unemp.year.w.diff$unemp







## the code below is an attempt to use the blscraper package to get the unemp data.
## it is pretty unruly and for the sake of reproducibility, using the url for the unemp data is pretty good.
##
## below is an attempt to get the table ids into the right format so that they referernce actual LAUS tables,
## here: https://download.bls.gov/pub/time.series/la/ is the site with the naming conventions
## add leading zeros to state fips < 10
# unemp.year$fips_length <- sprintf("%02d",unemp.year$state.fips) # fix to 2 characters 
# # paste onto end of laus code 
# unemp.year$laus.code1 <- paste0("LAU", unemp.year$laus.code, unemp.year$fips_length)
# ###################?paste0
# # the above is done using a txt file which has all the unemployment data for MSAs
# # next we're going to try to find the unemployment using the API.
# 
# # install.packages("blscrapeR")
# library(blscrapeR)
# BLS_KEY='92882bc0dd354a94b65f8b8a232a0b42'
# 
# # try to create a list of table names
# # feed this into the call to the bls api
# tables <- unemp.year[["laus.code1"]]
# 
# 
# test <- lapply(tables[1:25], 
#        function(j) (
#          bls_api(j, startyear = 2015, endyear = 2015)
#        ))
# #
# ?bls_api
# test1 <- bls_api("LAUMT011150000000003", startyear = 2015, endyear = 2015)
# # this works but it's arranged funny. 
# TEST2 <- bls_api(c("LAUBS060000000000003","LAUCN040010000000006"), startyear = 2015, endyear = 2015)
# 
# n <- 1:18
# n
# sprintf(paste0("e with %2d digits = %.", n, "g"), n, exp(1))
