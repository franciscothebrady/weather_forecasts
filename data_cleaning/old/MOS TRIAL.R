#MOS
library(data.table)
setwd("C:/Users/franc/OneDrive/Documents/Research/Weather Forecasts")
#Reading in the MOS data using fixed widths of 4 for each column
mos <- read.fwf("mso-trial.txt", skip = 3, widths = c(4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4))

mos <- mos[-c( 10, 11, 12, 13, 23, 24, 25, 26),]
# trim whitespace
mos$V1 <- trimws(mos$V1, which = "both")

mos <- mos[(mos$V1 == "TMP" | mos$V1 == "Q12" | mos$V1 == "Q24"),]


mos2 <- lapply(mos, function(x) {
  gsub("\\|", "", x)
})

View(mos2)
mos2 <- data.frame(mos2)
str(mos2)

# nowhitespace <- lapply(mos2, function(y) {
#   trimws(mos2, which = "both")
# })
# View(nowhitespace)

mos2[, c(2:16)] <- sapply(mos2[, c(2:16)], as.numeric)
str(mos2)
