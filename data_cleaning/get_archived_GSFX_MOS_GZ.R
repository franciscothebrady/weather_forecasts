get_archived_GFSX_MOS <- function(ui_station_id, ui_runtime_date, ui_runtime_hour) {
  
  #
  # function that outputs user specified archived MOS on NWS MDL server
  # author: Brian Seok [02 June 2017]
  #
  # input parameters:
  #     ui_station_id    : ICAO (e.g. "KDEN")
  #     ui_runtime_date  : YYYYMMDD (e.g. "20160523")
  #     ui_runtime_hour  : "00Z" or "12Z"
  #
  # output:
  #     data frame of MOS output (excluding CLIMO data)
  #
  
  require(plyr)
  require(stringr)
  require(readr)
  require(lubridate)
  
  # parse input parameters
  station_id    <- str_to_upper(ui_station_id)
  runtime_year  <- str_sub(ui_runtime_date, 1, 4)
  runtime_month <- str_sub(ui_runtime_date, 5, 6)
  runtime_day   <- str_sub(ui_runtime_date, 7, 8)
  runtime_hour  <- str_to_lower(ui_runtime_hour)
  
  # check if raw archived MOS file is already on local disk
  mos_file <- paste0("mex", runtime_year, runtime_month, ".t", runtime_hour, ".gz")
  local_mos_file <- paste0("data/", mos_file)
  if (!file.exists(local_mos_file)) {  # if file does not exist get it!
    # create directory to save downloaded file
    dir.create("data", showWarnings = FALSE)
    # get raw archived MOS file
    base_url <- "http://www.mdl.nws.noaa.gov/~mos/archives/mrfmex/"
    download_url <- paste0(base_url, mos_file)
    download.file(download_url, local_mos_file)
  }
  
  # read mos file into memory
  mos_outputs <- read_lines(gzfile(local_mos_file), skip = 2)
  
  # reformat user input of runtime date
  runtime_date <- paste0(str_replace(runtime_month, "^0+(?=[1-9])", ""), "/",
                         runtime_day, "/",
                         runtime_year)
  
  # locate block of MOS output that user wants in file
  block_start <- which(str_detect(mos_outputs, station_id) &
                         str_detect(mos_outputs, runtime_date))
  if (!any(block_start)) {
    return(NULL)
  } else {
    
    block_end   <- block_start +
      str_which(mos_outputs[block_start:length(mos_outputs)], "\\s{70}")[1] - 2
    
    # user selected MOS output
    mos_chr <- mos_outputs[block_start:block_end]
    
    # set start and end vectors (NOTE: we are ignoring CLIMO columns)
    start <- seq(1, 32) + rep(seq(0, 30, 2), each = 2)
    end   <- seq(1, 32) + c(0, rep(seq(2, 30, 2), each = 2), 32)
    
    # parse each string in mos_chr
    x1 <- lapply(mos_chr[c(2, 4:length(mos_chr))], function(s){ str_sub(s, start = start, end = end) })
    
    # drop unwanted elements
    y1 <- lapply(x1, function(s){ s[seq(2, 32, 2)] })
    
    # convert list to data frame
    z1 <- ldply(y1)
    
    # assign data frame column names (forecast hours)
    colnames(z1) <- y1[[1]]
    
    # assign data frame row names (forecast elements)
    rownames(z1) <- z1[,1]
    
    # remove first column and row that contains column and row names
    mos_df <- z1[-1,-1]
    
    # transpose data frame
    mos_df <- data.frame(t(mos_df), stringsAsFactors = FALSE)
    row.names(mos_df) <- NULL
    
    # append forecast times in hr
    fc_t <- data.frame(colnames(z1), stringsAsFactors = FALSE)
    fc_t <- as.numeric(fc_t[-1,])
    mos_df <- cbind.data.frame(FCDT=fc_t, mos_df)
    
    # append runtime in ymd_hms format
    rt_t1 <- rep(paste0(runtime_year, runtime_month, runtime_day), 15)
    rt_t2 <- rep(runtime_hour, 15)
    rt_t2 <- gsub(".$", "", rt_t2)  # remove Zs, but remember we're still in UTC/Zulu
    rt_t3 <- ymd_hms(paste(rt_t1, paste0(rt_t2, ":00:00")), tz="Zulu")
    mos_df <- cbind.data.frame(RTDT=rt_t3, mos_df)
    
    # convert forecast times to ymd_hms format
    mos_df$FCDT <- mos_df$RTDT + hours(mos_df$FCDT)
    
    # append station id
    mos_df <- cbind.data.frame(ICAO=rep(station_id, 15), mos_df, stringsAsFactors = FALSE)
    
    # final version of data frame we can use to do stuff
    return(mos_df)
    
  }
}