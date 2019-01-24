library(readr)

## county building permits by county
# from: https://www.census.gov/construction/bps/
# desc: This page provides data on the number of new 
# housing units authorized by building permits. 
# Data are available monthly, year- to- date, and annually 
# at the national, state, selected metropolitan area, county
# and place levels. The data are from the Building Permits Survey.

## notes: need to figure out weights (?)
## this data is monthly, so needs to be aggregated up probably.
## a script to download all the "c" files from the site makes sense.


data <- read_csv("https://www2.census.gov/econ/bps/County/co1001c.txt",
                 skip = 3, col_names = F)
