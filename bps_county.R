library(readr)
library(dplyr)
library(rvest)
## construction building permits by county
# from: https://www.census.gov/construction/bps/
# desc: This page provides data on the number of new 
# housing units authorized by building permits. 
# Data are available monthly, year- to- date, and annually 
# at the national, state, selected metropolitan area, county
# and place levels. The data are from the Building Permits Survey.

## notes: need to figure out weights (?)
## this data is monthly, so needs to be aggregated up probably.
## a script to download all the "c" files from the site makes sense.

### scan the page for the urls 

url <- "https://www2.census.gov/econ/bps/County/"
# read census page
page <- read_html(x = url)
# pull urls
links <- page %>%
  html_nodes("a") %>%
  html_attr("href") 
# only want files of the form coYYMMc.txt
# subset by "c.txt" to get the monthly files
# https://www2.census.gov/econ/bps/County/co0001c.txt
links <- links[grepl("c.txt", x = links)]
links <- paste0("https://www2.census.gov/econ/bps/County/", links)
# docs: https://www2.census.gov/econ/bps/Documentation/cntyasc.pdf
# building permits documentation codes
# 101 sf housing
# 103 2 family housing
# 104 3-4 family buildings 
# 105 5+ family buildings
# 109 totals
data_list <- list()

for (i in 1:length(links)) {
  # read in data
  data <- read_csv(links[i],
                   skip = 3, 
                   col_names = F)
  data_list[[i]] <- data
  }
# turn list to df
permits <- dplyr::rbind_list(data_list)
## format date variable
permits <- permits %>%
  mutate(X1 = lubridate::ymd(paste0(X1, "01")))

vars <- c("date","st_fips","cty_fips",
          "census_reg", "census_div", "county",
          "sfi_builds", "sfi_units", "sfi_values", # 101 imputed
          "f2i_builds", "f2i_units", "f2i_values", # 103 imputed
          "f3i_builds", "f3i_units", "f3i_values", # 104 imputed
          "mfi_builds", "mfi_units", "mfi_values", # 105 imputed
          "sfr_builds", "sfr_units", "sfr_values", # 101 reported
          "f2r_builds", "f2r_units", "f2r_values", # 103 reported
          "f3r_builds", "f3r_units", "f3r_values", # 104 reported
          "mfr_builds", "mfr_units", "mfr_values") # 105 reported
          

names(permits) <- vars

# drop state totals 
permits <- permits %>%
  filter(cty_fips != "000") 

### figure out other weird aspects of the data? 