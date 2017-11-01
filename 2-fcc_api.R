# francisco 
# finding census block codes and comparing fips codes 

# WHAT THIS PROGRAM DOES:
# 1. reads in 2010-16 storm events
# 2. builds a loop using the fcc api to return census 
#    block codes for all events. 
# 3. pauses the API request every 1000 calls
# 4. performs checks on the requests to make sure they're not too far off.
# 5. saves those checks (because they might be useful later)
# 6. cbinds the relevant results to the dataset and writes to csv.

api_append <- function() {
  
  library(dplyr)
  library(lubridate)
  
  # read in colnames and data
  header_names <- read.csv("data/colnames.csv", header = FALSE)
  events <- fread("data/1_events.csv")
  header_names <- t(header_names)
  # assign colnames to data
  colnames(events) <- header_names
  
  print(head(events))
  
  #apply FCC Census Block API from: https://www.fcc.gov/general/census-block-conversions-api
  # set up empty df for the response
  tracts <- data.frame(census.block = integer(),
                   fcc.county.FIPS = integer(),
                   fcc.county.name = character(),
                   state.FIPS = integer(),
                   state.code = integer(),
                   state.name = character(),
                   status     = character(),
                   executionTime = integer(),
                   stringsAsFactors = FALSE)
  
  # length of 1_events
  n_events <- length(events$EVENTS.begin_lat)
  count <- 1
  # for loop to fill in request 
  for (i in 1:n_events) {
    # set up the url and parameters
    url <- "http://data.fcc.gov/api/block/find?format=json"
  
    # set up lat/lon
    latitude <- events$EVENTS.begin_lat[i]
    longitude <- events$EVENTS.begin_lon[i]
    # run request
    request <- fromJSON(paste0(url, "&latitude=", latitude, "&longitude=", longitude, "&showall=false"))
    
    # print request
    print(head(request))
    
    
    tracts[i,] <- as.data.frame.list(request, stringsAsFactors = FALSE)

    print(head(tracts))
    count <- count + 1
    
    if (count==1000) {
      count <- 1
      Sys.sleep(3601)
    }

  }
  
  events <- cbind(events, tracts)

  # remove some the extra columns returned by the API call
  events$status <- NULL
  events$executionTime <- NULL
  
  print(tail(events))
  
  # sanity check: compare the fips code returned by the API to the original fips from the dataset
  matches <- as.numeric(substr(events$fcc.county.FIPS, 3, 5)) == as.numeric(events$EVENTS.czfips)		
  table(as.numeric(substr(events$fcc.county.FIPS, 3, 5)) == as.numeric(events$EVENTS.czfips))		    
  
  print(paste0(round((sum(matches)/nrow(events))*100,3),"% matched correctly"))
  
  
  # write to csv
  print("writing to .csv")
  write.csv(colnames, "data/2_fcc.api.csv", append = FALSE, row.names = FALSE)
  
}

api_append()
