
library(jug)
library(MazamaSpatialUtils)

# For use from the command line:
###setSpatialDataDir("~/data/Spatial")

# For use with the docker image built with Dockerfile:
setSpatialDataDir("/home/mazama/data/Spatial")

loadSpatialData('TMWorldBorders')
loadSpatialData('NaturalEarthAdm1')
loadSpatialData('USCensusCounties')
loadSpatialData('WorldTimezones')
loadSpatialData('WBDHU2')
loadSpatialData('WBDHU4')
loadSpatialData('WBDHU6')
loadSpatialData('WBDHU8')
loadSpatialData('WBDHU10')
loadSpatialData('WBDHU12')

jug() %>%
  
  # Countries
  get("/spatialUtils/getCountry", function(req, res, err){
    
    lons <- eval(parse(text=req$params$lons))
    lats <- eval(parse(text=req$params$lats))
    
    df <- getCountryCode(lons, lats, allData=TRUE)
    df$longitude <- round(lons, digits=2)
    df$latitude <- round(lats, digits=2)
    df <- df[,c('longitude','latitude','countryName','countryCode','ISO3','FIPS')]
    result <- readr::format_csv(df)
    
    res$content_type("text/csv")
    
    return(result)
    
  }) %>%
  
  # HUCs
  get("/spatialUtils/getHUC2", function(req, res, err){
    
    str(req$params)
    lons <- eval(parse(text=req$params$lons))
    lats <- eval(parse(text=req$params$lats))
    
    df <- getHUC(lons, lats, WBDHU2, allData=TRUE)
    df$longitude <- round(lons, digits=2)
    df$latitude <- round(lats, digits=2)
    df <- df[,c('longitude','latitude','countryCode','allStateCodes','HUCName')]
    result <- readr::format_csv(df)
    
    res$content_type("text/csv")
    
    return(result)
    
  }) %>%
  
  # HUCs
  get("/spatialUtils/getHUC4", function(req, res, err){
    
    str(req$params)
    lons <- eval(parse(text=req$params$lons))
    lats <- eval(parse(text=req$params$lats))
    
    df <- getHUC(lons, lats, WBDHU4, allData=TRUE)
    df$longitude <- round(lons, digits=2)
    df$latitude <- round(lats, digits=2)
    df <- df[,c('longitude','latitude','countryCode','allStateCodes','HUCName')]
    result <- readr::format_csv(df)
    
    res$content_type("text/csv")
    
    return(result)
    
  }) %>%
  
  # HUCs
  get("/spatialUtils/getHUC6", function(req, res, err){
    
    str(req$params)
    lons <- eval(parse(text=req$params$lons))
    lats <- eval(parse(text=req$params$lats))
    
    df <- getHUC(lons, lats, WBDHU6, allData=TRUE)
    df$longitude <- round(lons, digits=2)
    df$latitude <- round(lats, digits=2)
    df <- df[,c('longitude','latitude','countryCode','allStateCodes','HUCName')]
    result <- readr::format_csv(df)
    
    res$content_type("text/csv")
    
    return(result)
    
  }) %>%
  
  # HUCs
  get("/spatialUtils/getHUC8", function(req, res, err){
    
    str(req$params)
    lons <- eval(parse(text=req$params$lons))
    lats <- eval(parse(text=req$params$lats))
    
    df <- getHUC(lons, lats, WBDHU8, allData=TRUE)
    df$longitude <- round(lons, digits=2)
    df$latitude <- round(lats, digits=2)
    df <- df[,c('longitude','latitude','countryCode','allStateCodes','HUCName')]
    result <- readr::format_csv(df)
    
    res$content_type("text/csv")
    
    return(result)
    
  }) %>%
  
  # HUCs
  get("/spatialUtils/getHUC10", function(req, res, err){
    
    str(req$params)
    lons <- eval(parse(text=req$params$lons))
    lats <- eval(parse(text=req$params$lats))
    
    df <- getHUC(lons, lats, WBDHU10, allData=TRUE)
    df$longitude <- round(lons, digits=2)
    df$latitude <- round(lats, digits=2)
    df <- df[,c('longitude','latitude','countryCode','allStateCodes','HUCName')]
    result <- readr::format_csv(df)
    
    res$content_type("text/csv")
    
    return(result)
    
  }) %>%
  
  # HUCs
  get("/spatialUtils/getHUC12", function(req, res, err){
    
    str(req$params)
    lons <- eval(parse(text=req$params$lons))
    lats <- eval(parse(text=req$params$lats))
    
    df <- getHUC(lons, lats, WBDHU12, allData=TRUE)
    df$longitude <- round(lons, digits=2)
    df$latitude <- round(lats, digits=2)
    df <- df[,c('longitude','latitude','countryCode','allStateCodes','HUCName')]
    result <- readr::format_csv(df)
    
    res$content_type("text/csv")
    
    return(result)
    
  }) %>%
  
  # States
  get("/spatialUtils/getState", function(req, res, err){
    
    str(req$params)
    lons <- eval(parse(text=req$params$lons))
    lats <- eval(parse(text=req$params$lats))
    
    df <- getStateCode(lons, lats, allData=TRUE)
    df$longitude <- round(lons, digits=2)
    df$latitude <- round(lats, digits=2)
    df <- df[,c('longitude','latitude','countryName','stateName','countryCode','stateCode','fips','gns_lang')]
    names(df) <- c('longitude','latitude','countryName','stateName','countryCode','stateCode','FIPS','languageCode')
    result <- readr::format_csv(df)
    
    res$content_type("text/csv")
    
    return(result)
    
  }) %>%
  
  # Timezones
  get("/spatialUtils/getTimezone", function(req, res, err){
    
    str(req$params)
    lons <- eval(parse(text=req$params$lons))
    lats <- eval(parse(text=req$params$lats))
    
    df <- getTimezone(lons, lats, allData=TRUE)
    df$longitude <- round(lons, digits=2)
    df$latitude <- round(lats, digits=2)
    df <- df[,c('longitude','latitude','timezone','countryCode','UTC_offset','UTC_DST_offset')]
    result <- readr::format_csv(df)
    
    res$content_type("text/csv")
    
    return(result)
    
  }) %>%
  
  # Handle errors
  simple_error_handler_json() %>%

  # Serve up the results
  serve_it(host = Sys.getenv("JUG_HOST"), port = as.integer(Sys.getenv("JUG_PORT")))

# ----- END -------------------------------------------------------------------

