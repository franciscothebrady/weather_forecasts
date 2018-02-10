library(readr)
library(tidyverse)
library(reshape2)
library(lubridate)
bls_vars <- read_csv("/href/research7/m1fmb02/rstuff/bls_vars.csv")
# extract start date
start_date <- min(bls_vars$date)
# convert to wide format
bls_wide <- dcast(bls_vars, date ~ series.id, value.var = "unemp")
# sanity check
table(is.na(bls_wide))
# turn into ts object 
unemp.ts <- ts(bls_wide[,-1], start = c(2010,1), frequency = 12) # this didn't like start = start_date for some reason.
# seasonally adjust!
library(seasonal)
# this works but on all 1400 counties. too much!
unemp.seas <- seas(unemp.ts)
plot(unemp.seas)
#  you can subset by columns
unemp_small <- seas(unemp.ts[, c(1:10)])
plot(unemp_small)
# the trick is to subset the df before it's turned into a seas object
seas(unemp.ts[,c(grep(unemp.ts,'LAUCN0401300000000'))]) # doesn't work

maricopa.adj <- seas(ts(bls_wide$LAUCN040130000000003, start = c(2010,1), frequency = 12)) # works
plot(maricopa.adj)
