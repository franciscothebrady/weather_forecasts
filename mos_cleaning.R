# francisco 
# reading in fwf with MOS output
# this is weather forecast data from: 

# install.packages("easypackages") this package allows you to load multiple packages at once
library(easypackages)
libraries("readr","dplyr","reshape2","data.table")

# set wd
getwd()
setwd("C:/Users/franc/OneDrive/Documents/Research/Weather Forecasts")

# read in file of MOS output
## method 1
# mos <- read.fwf("mos_output.txt", skip = 3, widths = c(4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4)) # this is real slow

## method 2
# readr::read_fwf did the entire thing in a few seconds
# note that I specified both "" and "NA" as NAs, meaning the lines between stations become NAs
mos_output <- read_fwf("mos_output.txt", fwf_widths(c(5,4,4,4,4,4,4,4,4,4,4,4,4,4,4,3,NA)), na = c("", "NA")) 
str(mos_output)

# removing all rows of just NAs
mos_output <- mos_output[rowSums(is.na(mos_output)) != ncol(mos_output),]

# remove empty top row
mos_output <- mos_output[-1,]

# function to remove whitespace around obs
mos_output <- data.frame(lapply(mos_output, trimws))
head(mos_output)

# remove "\\|" from observations
mos_output <- data.frame(lapply(mos_output, function(x) {
  gsub("\\|", "", x)
}))

### pseudocode
# for every 12 lines (station name) take that obs and paste station name into new col for the next 12 lines
# for every 12 lines (paste runtime date from top) take that and paste runtime date into new col for the next 12 lines
# THEN melt using station name and date as ID vars

)


#### testing out how to melt data

test <- mos_output[1:100,]
library(reshape2)
test1 <- melt(test)
# reshaping by certain variables (didnt work)
test2 <- reshape(test1, idvar = "X1", timevar= "X2", direction = "wide")
# transposing df (worked better sort of)
test3 <- data.frame(t(test1))
# taking the first row values and making them the colnames
names(test3) <- as.matrix(test3[1, ])
test3 <- test3[-1, ]
test3[] <- lapply(test3, function(x) type.convert(as.character(x)))
test3
# trying to paste the dates into one column 
adddates <- test
adddates$runtime <- NULL
test$X7 <- (as.character(test$X7))
test$X8 <- (as.character(test$X8))
test$X9 <- (as.character(test$X9))
adddates$runtime <- paste(test$X7[1],test$X8[1],test$X9[1], sep = "")

# trying to come up with a loop to grab the station names
# which we can then use to separate the tables
station.names <- (for i in seq(1, 100, by = 12)){
  list(names(test3[]))
}

View(station.names)

# looking not too bad!
?paste

# removing vars that don't have to do with temp, QPF12 and 24
## might want to wait on this because it also removes forecast hour (FHR) and MOS station
# mos <- mos[(mos$V1 == "TMP" | mos$V1 == "Q12" | mos$V1 == "Q24"),]

# changing cols to numeric
#mos2[, c(2:16)] <- sapply(mos2[, c(2:16)], as.numeric)
#str(mos2)
####
