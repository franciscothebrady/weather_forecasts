# francisco
# downloading sheldus data
#
# what this file does:
# 1. grabs sheldus zip files from gdrive
# 2. unzips, cleans and combines them
# 3. gets them ready to be merged with events data
library(readr)
library(lubridate)
#install.packages("quantmod")
library(quantmod)

options(scipen = 999) # Do not print scientific notation
options(stringsAsFactors = FALSE) # Do not load strings as factors

setwd("~/weather_forecasts/")
# load in sheldus data
# all 50 states, [fill this in with the rest of the SHELDUS parameters]
A1 <- read_csv("~/weather_forecasts/data/SHELDUS/UID12986f_AGG_A.csv")
A2 <- read_csv("~/weather_forecasts/data/SHELDUS/UID12986f_AGG_A2.csv")
A3 <- read_csv("~/weather_forecasts/data/SHELDUS/UID12986f_AGG_A3.csv")
A4 <- read_csv("~/weather_forecasts/data/SHELDUS/UID12986f_AGG_A4.csv")
A5 <- read_csv("~/weather_forecasts/data/SHELDUS/UID12986f_AGG_A5.csv")
# combine all the files together
sheldus <- rbind(A1, A2, A3, A4, A5)

sheldus$`County FIPS` <- substring(sheldus$`County FIPS`, 2, 6)

# combine prop and crop values for total damages
sheldus$total_adj_2009_dmg <- sheldus$`CropDmg(ADJ 2009)` + sheldus$`PropertyDmg(ADJ 2009)`

# read in events data for merging
events <- read_csv("data/3_econ-vars.csv")
events$fcc.county.FIPS
events$Year <- as.integer(year(events$EVENTS.begin_date))
events$Month <- as.integer(month(events$EVENTS.begin_date))

# use CPI to adjust events data, (2009 base year)
getSymbols("CPIAUCSL", src='FRED') #Consumer Price Index for All Urban Consumers: All Items
avg.cpi <- apply.yearly(CPIAUCSL, mean)

cf <- avg.cpi/as.numeric(avg.cpi['2009'])
#cf <- as.data.frame(cf)
events$adjusted2009damage_value <- data.frame(adjusted2009damage_value= matrix(unlist(lapply(events$Year, function(x){
  as.data.frame(cf$CPIAUCSL[match(x, year(cf[,1]))])[,1]
  })),nrow=8448,byrow=T)) * events$EVENTS.damage_value


events_sheldus <- merge(x = events, y = sheldus, by.x = c("Year", "Month", "fcc.county.FIPS"),
                        by.y = c("Year", "Month", "County FIPS"))


diff <- as.data.frame(events_sheldus$total_adj_2009_dmg - 
                        (events_sheldus$adjusted2009damage_value*10^events_sheldus$EVENTS.damage_magnitude))
events_sheldus[diff == min(diff),]
summary(diff)



bls_test <- bls_vars[sample(1:8448, 1000, replace=F),]
?sample

blah <- split(bls_test,f = "series.id")
library(reshape2)

bls_cast <- dcast(bls_test, date ~ series.id, value.var = "unemp", fun.aggregate = mean, na.rm=T)
bls_cast[1,]
