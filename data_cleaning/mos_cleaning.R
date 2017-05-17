# francisco 
# reading in fwf with MOS output
# this is weather forecast data from: 

# install.packages("easypackages") this package allows you to load multiple packages at once
library(easypackages)
libraries("readr","dplyr","reshape2","data.table","stringr")

# set wd
getwd()
setwd("C:/Users/franc/OneDrive/Documents/Research/Weather Forecasts")

# read in file of MOS output
## method 1
# mos <- read.fwf("mos_output.txt", skip = 3, widths = c(4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4)) # this is real slow

## method 2
# readr::read_fwf did the entire thing in a few seconds
# note that I specified both "" and "NA" as NAs, meaning the lines between stations become NAs
# options(stringsAsFactors = FALSE)
# mos_output <- read_fwf("mos_output.txt", fwf_widths(c(5,4,4,4,4,4,4,4,4,4,4,4,4,4,4,3,NA)), na = c("", "NA"), skip = 2) 
# str(mos_output)
# ?read_fwf

# removing all rows of just NAs
# mos_output <- mos_output[rowSums(is.na(mos_output)) != ncol(mos_output),] # may not be useful

# remove empty top row
# mos_output <- mos_output[-1,] # solved by skip = 

# function to remove whitespace around obs
# mos_output <- data.frame(lapply(mos_output, trimws))
# head(mos_output)
# 
# # remove "\\|" from observations
# mos_output <- data.frame(lapply(mos_output, function(x) {
#   gsub("\\|", "", x)
# }))
# 
# head(mos_output,15)

# construct a new df, taking rows and turning them into columns
# new <- data.frame(t(mos_output$X1))
# 
# newdf <- as.data.frame(matrix(unlist(new, use.names=FALSE),ncol=13, byrow=TRUE))
# 
# # list of NA positions, could use this to set where to break rows into new columns? if divisible by 13 or something
# na_positions <- which(is.na(new))
# head(na_positions)
# stations <- which(apply(new, 2, function(x) any(grepl("[P]{4}", x))))
# 
# new[18440]
# stacked <- stack(mos_output)[1:100,]
# removing vars that don't have to do with temp, QPF12 and 24 might want to wait on this 
# because it also removes forecast hour (FHR) and MOS station
# mos <- mos[(mos$V1 == "TMP" | mos$V1 == "Q12" | mos$V1 == "Q24"),]

# method 3: read line by line using readLines
mos_output <- readLines("mos_output.txt")
mos_output <- trimws(mos_output, which = "both")
head(mos_output)

# remove | from strings
mos_output <- gsub("\\|", " ", mos_output)
mos_output[1:10]

# extract forecast runtime dates
dt_pattern <- "(\\d+/\\d+/\\d+\\s+\\d+)"
runtimes <- str_match(mos_output, dt_pattern)
runtimes[1:20]
# remove NAs
runtimes <- na.omit(runtimes)

# extract station names
st_pattern <-  "(^[[:upper:]]{4})"
st_names <- str_match(mos_output, st_pattern)
st_names <- na.omit(st_names)
head(st_names)

# # TEST: combining runtimes and station names as columns in df! (repeating both values n times)
# small_runtimes <- rep(runtimes[1:10], 5)
# small_stnames <- rep(st_names[1:10], 5)
# df <- data.frame(small_runtimes, small_stnames)
# df
# rm(df)
## seems like it worked, want to double check on how its repeating (by element or cycling through)

# extract valid forecast times
vf_pattern <- "FHR  24  36  48  60  72  84  96 108 120 132 144 156 168 180 192"
# pattern attempt: "FHR\\s+\\d+9repeat 15x" we can figure out the pattern later
vf_names <- str_match(mos_output, vf_pattern)
head(vf_names, 40)
vf_names <- na.omit(vf_names)
# split into individual elements
vf_names <- str_split(vf_names, "\\s+")
vf_names[1:10]

mos_output[1:10]
# extract TEMP
# pattern is some number of spaces followed by a number? 
mos_output[6]
val_pattern <- "\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+"
tmp_pattern <- paste0("^TMP", val_pattern)
tmp_values <- str_match(mos_output, tmp_pattern)
tmp_values <- na.omit(tmp_values)
tmp_values[1:10]
# REMEMBER 999 = missing data (double check later)
tmp_values <- str_split(tmp_values, "\\s+")

mos_output[1:40]
# extract qpf 12 and 24
q12_pattern <- "Q12\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+"
q12_values <- str_match(mos_output, q12_pattern)
q12_values <- na.omit(q12_values)
q12_values <- paste(q12_values, "999 999 999")
q12_values[1:40]
q12_values <- str_split(q12_values, "\\s+")

# combine everything into  something

test_combine <- as.data.frame(cbind(runtimes, st_names, vf_names, tmp_values, q12_values))
