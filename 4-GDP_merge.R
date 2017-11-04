# 

# 1. read in weather events csv
# 2. access gdp with bea API
# 3. merge gdp + events
# 4. MABLE stuff


#   -- get real GDP by MSA for 2010-2016
#   -- (https://www.bea.gov/API/bea_web_service_api_user_guide.htm)   check docs for YEAR
beaSpecs <- list(
  "UserID" = # ,
    "method" = "GetData",
  "datasetname" = "RegionalProduct",
  "Component" = "RGDP_MAN",
  "IndustryId" = "1",
  "GeoFIPS" = "MSA",
  "Year" = "2010,2011,2012,2013,2014,2015,2016",   
  "ResultFormat" = "json"                          
)
gdp_msa <- beaGet(beaSpecs, asWide = FALSE)
rm(beaSpecs)
