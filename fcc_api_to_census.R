# francisco 
# using the FCC API to match lat/lon to census tracts
# census block conversion API docs here: https://www.fcc.gov/general/census-block-conversions-api

# install.packages("easypackages") this package allows you to load multiple packages at once
library(easypackages)
libraries("httr","jsonlite","dplyr")

options(stringsAsFactors = FALSE)

# set wd
getwd()
setwd("C:/Users/franc/OneDrive/Documents/Research/Weather Forecasts/Script")

# i'm not using the actual data here. replace with actual lat/lons later!
storms <- read.csv(paste0(dirname(getwd()), "/storm_events_2015.csv"))
storms <- filter(storms, BEGIN_LAT != "NA")
storms <- head(storms, 100)
                   
# following this as an example: http://tophcito.blogspot.com/2015/11/accessing-apis-from-r-and-little-r.html#fn2
# set up the url and parameters

url <- "http://data.fcc.gov/api/block/find?format=json"


latitude <- storms$BEGIN_LAT

longitude <- storms$BEGIN_LON
request <- paste0(url, "&latitude=", latitude, "&longitude=", longitude, "&showall=false")

str(request)
# use the names of the lists as the names for the df to make

tracts <- data.frame(FIPS = rep(0, 100),
                     County.FIPS = rep(0, 100),
                     County.name = rep(0, 100),
                     State.FIPS = rep(0, 100),
                     State.code = rep(0, 100),
                     State.name = rep(0, 100),
                     status     = rep(0, 100),
                     executionTime = rep(0, 100))


#  something about the way this request works requires the df setup beforehand so it fills in as.data.frame nicely.
for (i in 1:100) {
  latitude <- storms$BEGIN_LAT[i]
  longitude <- storms$BEGIN_LON[i]
  request <- fromJSON(paste0(url, "&latitude=", latitude, "&longitude=", longitude, "&showall=false"))
  tracts[i,] <- as.data.frame.list(request)
}

