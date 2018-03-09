library(readr)
library(tidyverse)
library(reshape2)
library(lubridate)



bls_vars <- read_csv("data/bls_vars.csv")
# extract start date
start_date <- min(bls_vars$date)
# convert to wide format
bls_wide <- dcast(bls_vars, date ~ series.id, value.var = "unemp")
# create a list of dataframes split on the series IDs
bls_split <- split.data.frame(bls_vars, f = bls_vars$series.id)

# apply seasonal adjustment to each list element
library(seasonal)
# first turn the list into ts objects
unemp_ts <- lapply(bls_split, function(x) { ts(x$unemp, 
                                               start = c(2010,1),
                                               frequency = 12)})

# use lapply again to seasonally adjust

# unemp_adju <- lapply(unemp_ts, seas(regression.aictest = NULL)) # r didnt like this
# threw an error because of trading days 

unemp_adju <- unemp_ts %>% lapply(. %>% seas(x11 = "", 
                                             regression.aictest = NULL)) 
# using default x11 specs
# then use map to morph back into a dataframe (remember to keep ids/listnames as a column)
library(purrr)
unemp_df <- map_df(unemp_adju, as.data.frame, .id = "series.id")
# take a look
head(unemp_df)
# i'm p sure final or seasonaladju is the one we want here. 
unemp_adju <- unemp_df %>% select(date, series.id, unemp = final) # this works as output 
# now we can work on adding this to the events from t thru t+6
write.csv(unemp_adju, "data/unemp_adju.csv", row.names = FALSE)

ggplot(unemp_adju, aes(x = date, y = unemp, group=series.id, color=series.id)) + 
  geom_line() + theme(legend.position = "none")
#### OLD STUFF ####
# # sanity check
# table(is.na(bls_wide))
# # turn into ts object 
# unemp.ts <- ts(bls_wide[,-1], start = c(2010,1), frequency = 12) # this didn't like start = start_date for some reason.
# # seasonally adjust!
# # install.packages("seasonal")
# library(seasonal)
# # this works but on all ~1400 counties. too much!
# unemp.seas <- seas(unemp.ts)
# plot(unemp.seas)
# 
# # the gsdc package looks promising to convert the ts object back into a df for ggplot2 style visualizations
# # https://cran.r-project.org/web/packages/ggseas/vignettes/ggsdc.html
# rm(unemp.ts)
# #  you can subset by columns
# unemp_small <- seas(unemp.ts[, c(1:10)])
# plot(unemp_small)
# # the trick is to subset the df before it's turned into a seas object
# seas(unemp.ts[,c(grep(unemp.ts,'LAUCN0401300000000'))]) # doesn't work
# seas(ts(bls_wide$LAUCN040010000000003, start = c(2010,1), fre
# maricopa.adj <- seas(ts(bls_wide$LAUCN040130000000003, start = c(2010,1), frequency = 12)) # works
# plot(maricopa.adj)
# 
# # convert back into dataframe for plotting. 
# # save series names, which can be matched up with counties later
# series_names <- unemp.seas$x
# names(as.data.frame(unemp.seas))
# 
# series_we_want <- unique(events$series.id))
# 
# unemp_we_Want <- bls_wide %>% select(series_we_want)) # 
# split_test <- split(unemp_we_Want, f = unemp_we_Want$)