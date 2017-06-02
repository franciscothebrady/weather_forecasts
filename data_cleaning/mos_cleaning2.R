# francisco / brian
# mos cleaning test #x

library(easypackages)
libraries("readr","plyr","reshape2","data.table","stringr")

# set wd
getwd()
setwd("C:/Users/franc/OneDrive/Documents/Research/Weather Forecasts")

test_mos <- readLines("test_mos.txt")
test_mos <- trimws(test_mos, which = "both")
test_mos

# remove | from strings
test_mos <- gsub("\\|", " ", test_mos)

# extract valid forecast times
vf_pattern <- "FHR  24  36  48  60  72  84  96 108 120 132 144 156 168 180 192"
# pattern attempt: "FHR\\s+\\d+9repeat 15x" we can figure out the pattern later
vf_names <- str_match(test_mos, vf_pattern)
str(vf_names) # character vector
head(vf_names)
# vf_names <- na.omit(vf_names)
# split into individual elements
vf_names <- str_split(vf_names, "\\s+")

# as dataframe
vfcst_df <- as.data.frame(na.omit(unlist(vf_names)))
vfcst_df

# temps
val_pattern <- "\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+"
tmp_pattern <- paste0("^TMP", val_pattern)
tmp_values <- str_match(test_mos, tmp_pattern)

# unlist
tmp_values <- as.data.frame(na.omit(unlist(tmp_values)))

is.array(tmp_values)
# rbind
df <- cbind(vfcst_df, tmp_values)
df
# note: change names to FHR and TMP

val_pattern <- "\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+\\s+\\d+"
q12_pattern <- paste0("^Q12", val_pattern)
q12_values <- str_match(test_mos, q12_pattern)
str(q12_values)
q12_values <- na.omit(q12_values)
q12_values <- str_split(q12_values, "\\s+")
q12_values <- unlist(q12_values)




q12_values <- trimws(q12_values)
is.array(q12_values)
# q12_values <- paste(q12_values, "99 99 99")
q12_values <- as.array(q12_values)
dim(q12_values)

is.array(q12_values)


q12_values

df <- cbind(df, q12_values)

