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
  #     character vector of MOS output
  #
    
  require(stringr)
  require(readr)
  
  # parse input parameters
  station_id    <- str_to_upper(ui_station_id)
  runtime_year  <- str_sub(ui_runtime_date, 1, 4)
  runtime_month <- str_sub(ui_runtime_date, 5, 6)
  runtime_day   <- str_sub(ui_runtime_date, 7, 8)
  runtime_hour  <- str_to_lower(ui_runtime_hour)
  
  # check if raw archived MOS file is already on local disk
  mos_file <- paste0("mex", runtime_year, runtime_month, ".t", runtime_hour, ".gz")
  local_mos_file <- paste0("mos_data/", mos_file)
  if (!file.exists(local_mos_file)) {  # if file does not exist get it!
    # create directory to save downloaded file
    dir.create("mos_data", showWarnings = FALSE)
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
  block_end   <- block_start +
    str_which(mos_outputs[block_start:length(mos_outputs)], "\\s{70}")[1] - 2
  
  # return user selected MOS output
  mos_outputs[block_start:block_end]
}