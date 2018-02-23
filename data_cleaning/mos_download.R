# francisco
# download MOS files
# this script:
# 1. scrapes the nws mos archive
# 2. grabs the urls
# 3. downloads them all into a folder
library(tidyverse)
library(rvest)

# set url
mos_url <- "https://sats.nws.noaa.gov/~mos/archives/mrfmex/"

# read mos archive page
page <- read_html(mos_url) 

# extract mos urls 
urls <- page %>% html_nodes("td > a") %>%
  html_attr("href")

# first element is garbage
urls <- urls[2:365]

# paste url onto each element to set up download file function
prefix <- "https://sats.nws.noaa.gov/~mos/archives/mrfmex/"

urls <- paste0(prefix, urls)


# download files into data folder
for( i in 1:length(urls)){
  print(i)
  # add check for if(!file.exists)
  download.file(urls[i], paste0("data/", basename(urls)[i]))
  # also add filter to only download for the dates we need. 
  }


       #mapply(function(x,y) download.file(x,y),urls,files)
       
#download.file(url, destfile, method, quiet = FALSE, mode = "w",
              