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
library(dplyr)
library(bea.R)

# load processed 2015-2013 storm events, obs, and forecasts

fteen <- read.csv("data/2015_storm_events_processed.csv", stringsAsFactors = FALSE)
frteen <- read.csv("data/2014_storm_events_processed.csv", stringsAsFactors = FALSE)
thteen <- read.csv("data/2013_storm_events_processed.csv", stringsAsFactors = FALSE)
# remove extra columns (this should be done by name instead of index)
fteen$X <- NULL
frteen$X <- NULL
frteen$judge1 <- NULL
frteen$judge2 <- NULL
thteen$X <- NULL

combined_events <- rbind(fteen, frteen, thteen)

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

# change to whatever number of obs we end up having (2013-15 = 1697)
tracts <- data.frame(FIPS = rep(0, 1697),
                     County.FIPS = rep(0, 1697),
                     County.name = rep(0, 1697),
                     State.FIPS = rep(0, 1697),
                     State.code = rep(0, 1697),
                     State.name = rep(0, 1697),
                     status     = rep(0, 1697),
                     executionTime = rep(0, 1697))

nevents <- length(combined_events$EVENTS.begin_lat)
#  something about the way this request works requires the df setup beforehand so it fills in as.data.frame nicely.
for (i in 1:nevents) {
  latitude <- combined_events$EVENTS.begin_lat[i]
  longitude <- combined_events$EVENTS.begin_lon[i]
  request <- fromJSON(paste0(url, "&latitude=", latitude, "&longitude=", longitude, "&showall=false"))
  tracts[i,] <- as.data.frame.list(request)
}

tracts

# test whether all the calls worked...

table(tracts$status == "OK") # FALSE 36, TRUE 1661 # not sure what the issue is....


# cbind with combined_events

combined_events <- cbind(combined_events, tracts)
rm(frteen, fteen,thteen)
# tidy events - drop cols not needed now 
tidy_events <- combined_events[, c(2,36:41,16,17,33,34,35,20,21)]

#-- get real GDP by MSA for 2013-15
beakey <- "AF498701-0543-490E-B9B3-B850D6166872"



#-- (https://www.bea.gov/API/bea_web_service_api_user_guide.htm)
beaSpecs <- list(
  "UserID" = beakey,
  "method" = "GetData",
  "datasetname" = "RegionalProduct",
  "Component" = "RGDP_MAN",
  "IndustryId" = "1",
  "GeoFIPS" = "MSA",
  "Year" = "2013, 2014, 2015",
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


# list of all counties in each msa, from bea: https://www.bea.gov/regional/docs/msalist.cfm
# download as csv
msa_list <- read.csv("data_cleaning/metrolist.csv", header = F, sep = ",", fill = T, stringsAsFactors = F, strip.white = TRUE)
names(msa_list) <- c("CBSA.code","msa.name","fips.state.county","County.name")

# edit msa.name to match gdp/msa names
msa_list$msa.name <- gsub(" (Metropolitan Statistical Area)", "", msa_list$msa.name, fixed = TRUE)

msa_list <- separate(data = msa_list, col = County.name, into = c("County.name", "State"), sep = ",")
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
msa_list$state.abb[884] <- "VA"
msa_list$state.abb[1012] <- "VA"
msa_list$state.abb[1103] <- "VA"
msa_list$state.abb[1104] <- "VA"

# merge msa list and gdp 
gdp_msa$CBSA.code <- as.integer(gdp_msa$CBSA.code)
msa_list$CBSA.code <- as.integer(msa_list$CBSA.code)

gdp_msa_counties <- merge(gdp_msa, msa_list, by.x = c("CBSA.code","CBSA.title"), by.y = c("CBSA.code","msa.name"), allow.cartesian = TRUE)

## add full state names to gdpmsacountylist
# trim ws around st var
gdp_msa_counties$State <- trimws(gdp_msa_counties$State)

# create state name and abb lookup table
states <- data.frame(state.full=toupper(state.name), State = toupper(state.abb))
states <- bind_rows(states, data.frame(state.full="DISTRICT OF COLUMBIA", State="DC") )
# merge on the look up table
gdp_msa_counties <- merge(gdp_msa_counties, states, by = "State")

# extract year var in events, so we can merge by events, county, and state
library(lubridate)
tidy_events$EVENTS.year <- year(tidy_events$EVENTS.begin_date)
tidy_events$State.name <- toupper(tidy_events$State.name)
gdp_msa_counties$YEAR <- as.numeric(gdp_msa_counties$YEAR)
tidy_events$County.FIPS <- as.numeric(tidy_events$County.FIPS)
# merge gdp_msa_counties into combined events

gdp_tidy_events <- inner_join(gdp_msa_counties, tidy_events, by = c("fips.state.county"="County.FIPS",
                                                               "state.full"="State.name",
                                                               "YEAR"="EVENTS.year",
                                                               "State"="State.code"))


## recode damage vars
# use damage magnitude n as exponent value to multiply on damage amount.
gdp_tidy_events$EVENTS.damage_value <- gdp_tidy_events$EVENTS.damage_value*10^gdp_tidy_events$EVENTS.damage_magnitude
gdp_tidy_events$EVENTS.damage_magnitude <- NULL

# create a "forecast skill variable" - subtract forecasted prcp category from actual ghcnd prcp category
# for f2 (three day forecast)
gdp_tidy_events$skill.f2 <- gdp_tidy_events$Q24.f2 - gdp_tidy_events$GHCND.prcp_cat
# for f6 (seven day forecast)
gdp_tidy_events$skill.f6 <- gdp_tidy_events$Q24.f6 - gdp_tidy_events$GHCND.prcp_cat

# calculate gdp from MSA.GDP*10^MSA.GDP.magnitude
gdp_tidy_events$MSA.GPD.magnitude <- as.numeric(gdp_tidy_events$MSA.GPD.magnitude)
gdp_tidy_events$MSA.GDP.value <- gdp_tidy_events$MSA.GDP*10^gdp_tidy_events$MSA.GPD.magnitude

# calculate EVENTS.damage_value / MSA.GDP.value, which we'll then  log to plot
gdp_tidy_events$damage.over.gdp <- gdp_tidy_events$EVENTS.damage_value / gdp_tidy_events$MSA.GDP.value

# plot damage value over "skill score" for f2 using log/gdp
plot(gdp_tidy_events$skill.f2, gdp_tidy_events$damage.over.gdp)
# same thing with only non-zero damages
with(gdp_tidy_events[gdp_tidy_events$EVENTS.damage_value !=0, ],
     plot(gdp_tidy_events$skill.f2, log(gdp_tidy_events$damage.over.gdp)))
# with(df[df$var1 < N, ], plot( var1, var2))

# plot damage value over "skill score" for f6
# visualization of forecast accuracy with boxplots (forecast-obs on y axis, x axis is forecast category)
boxplot(skill.f2 ~ GHCND.prcp_cat, data = gdp_tidy_events, main = "36 Hour Forecast Quality", xlab = "Observed Precip. Categories", ylab = "Forecasted - Observed")
boxplot(skill.f6 ~ GHCND.prcp_cat, data = gdp_tidy_events, main = "192 Hour Forecast Quality", xlab = "Observed Precip. Categories", ylab = "Forecasted - Observed")

boxplot(skill.f2 ~ GHCND.prcp_cat, data = gdp_tidy_events, lwd = 2, ylab = "Forecast Accuracy")
stripchart(skill.f2 ~ GHCND.prcp_cat, vertical = TRUE, data = gdp_tidy_events,
           method = "jitter", add = TRUE, pch = 20, col = "blue")
# create summary tables for output for each year
# install.packages("stargazer")
library(stargazer)

gdp_tidy_events %>% filter(YEAR==2013) %>% 
  select(state.full, MSA.GDP.value, EVENTS.damage_value, CBSA.pop_density) %>% stargazer(type="html", title="2013", out="2013.html")

gdp_tidy_events %>% filter(YEAR==2014) %>% 
  select(state.full, MSA.GDP.value, EVENTS.damage_value, CBSA.pop_density) %>% stargazer(type="html", title="2014", out="2014.html")

gdp_tidy_events %>% filter(YEAR==2015) %>% 
  select(state.full, MSA.GDP.value, EVENTS.damage_value, CBSA.pop_density) %>% stargazer(type="html", title="2015", out="2015.html")

# were there any places where the skill was 0 (perfect) and the damage was 0?

test  <- gdp_tidy_events %>% filter(skill.f2==0, skill.f6==0, EVENTS.damage_value==0, Q24.f6==1) # %>%    stargazer(type="text", title="Perfect 7-day Forecasts, No Damage?", summary = FALSE)
View(test)
gdp_tidy_events %>% filter(skill.f2==0, skill.f6==0, EVENTS.damage_value==0, Q24.f2==1) %>% 
  select(EVENTS.begin_date, CBSA.title, MSA.GDP, skill.f2, skill.f6, GHCND.prcp_cat, EVENTS.damage_value) %>%
  stargazer(type="html", title="Perfect 3-day Forecasts, No Damage?", out="perfect_3day.html", summary = FALSE)
