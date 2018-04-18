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
  header_names <- read.csv("data/colnames.csv", header = FALSE)
  events <- read.csv("data/1_events.csv", header = FALSE)
  
  names(events) <- header_names
  
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
  nrain <- length(events$EVENTS.begin_lat)
  
  # for loop to fill in request 
  for (i in 1:nrain) {
    # set up the url and parameters
    url <- "http://data.fcc.gov/api/block/find?format=json"
  
    # set up lat/lon
    latitude <- events$EVENTS.begin_lat[i]
    longitude <- events$EVENTS.begin_lon[i]
  
    # run request
    request <- fromJSON(paste0(url, "&latitude=", latitude, "&longitude=", longitude, "&showall=false"))
    tracts[i,] <- as.data.frame.list(request, stringsAsFactors = FALSE)
  
    #### insert pause of 1 hour after every 1000 calls
    # Sys.sleep(abs(rnorm(1)*3))
    
  }
  
  events <- cbind(events, tracts)
  
  rm(tracts)
  
}

