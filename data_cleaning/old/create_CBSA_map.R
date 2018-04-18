##
## Example of how to create a CBSA map using Census TIGER Shapefile
## Download Census TIGER file at
## https://www.census.gov/cgi-bin/geo/shapefiles/index.php
##              - Select for 2010, Core Based Statistical Area
##                then select for Metropolitan/Micropolitan Statistical Area / US
##
## Repeat this for state or county file if you want to draw borders of state or county
##
## Unzip the downloaded file(s), which should be tl_2010_us_cbsa10.zip, tl_2010_us_state10.zip, etc.
## Set working directory to folder that contains the content of unzipped shapefiles
##

library(rgdal)
library(rgeos)
library(maptools)
library(ggplot2)
library(ggmap)

# set system local because a few characters in shapefile cause trouble
# when filtering out select US territories from being mapped
Sys.setlocale('LC_ALL', 'C')

# read shapefile
sf_cbsa <- "tl_2010_us_cbsa10.shp"
cbsa_map <- readOGR(sf_cbsa, layer = "tl_2010_us_cbsa10")
sf_state <- "tl_2010_us_state10.shp"
state_map <- readOGR(sf_state, layer = "tl_2010_us_state10")

# remove CBSAs, etc. of Alaska, Hawaii, and Puerto Rico
cbsa_map <- cbsa_map[!grepl("AK$|HI$|PR$", cbsa_map$NAME10), ]
length(cbsa_map)   # result should be 933 after remove those three areas
state_map <- state_map[!grepl("Alaska|Hawaii|Puerto", state_map$NAME10), ]
length(state_map)  # result should be 49 after removing those three areas

# make data ggplot friendly
cbsa_map_f <- fortify(cbsa_map, region = "GEOID10")
state_map_f <- fortify(state_map, region = "GEOID10")

# create map, draw object you want in the background first,
# i.e., state boundaries then CBSA boundaries
bdry_map <- ggplot(NULL) +
  geom_polygon(data = state_map_f, aes(long, lat, group = group), color = "black", fill = "white") +
  geom_polygon(data = cbsa_map_f, aes(long, lat, group = group), color = "blue", fill = "light grey") +
  theme_bw()

# output map
bdry_map
