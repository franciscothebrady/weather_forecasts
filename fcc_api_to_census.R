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
storms <- head(storms, 1000)
                   
# following this as an example: http://tophcito.blogspot.com/2015/11/accessing-apis-from-r-and-little-r.html#fn2
# set up the url and parameters

url <- "http://data.fcc.gov/api/block/find?format=json"

latitude <- storms$BEGIN_LAT[100]

longitude <- storms$BEGIN_LON[100]

request <- paste0(url, "&latitude=", latitude, "&longitude", longitude, "&showall=false")


# ok so i constructed the request url for the first obs
# test!

test <- GET(url = request)
test$status_code
test$content

test.content <- rawToChar(test$content)
test.content
# ok so this kinda worked but no results for the location...

jsontest <- fromJSON(request)
jsontest
class(jsontest)
str(jsontest)
# this is another method 

# now need to figure out how to loop or lapply over each entry. 

