#
# simple example how you might use the get_archive_GFSX_MOS function
# and go about parsing the output
#                                           -Brian
#

library(plyr)
library(dplyr)
library(tidyr)
library(stringr)

# load function
source("data_cleaning/get_archived_GFSX_MOS.R")

mos_output <- get_archived_GFSX_MOS("KDEN", "20151210", "00Z")
mos_output

# replace pipe character in mos_output
x <- str_replace_all(mos_output, "\\|", " ")
x

df1 <- data.frame(c(1:17))  # dummy data frame
# parse each character vector, use fix width = 4 characters
for (i in 1:19) {
  df1 <- cbind(df1, data.frame(str_match_all(x[i], ".{4}"), stringsAsFactors = FALSE))
}
# name data frame columns
names(df1) <- df1[1,]
# remove row containing names
df1 <- df1[-1,]
# remove columns ( -1: dummy, -2: title, -4: days)
df1 <- df1[,c(-1,-2,-4)]

# data frame of mos output
df1
