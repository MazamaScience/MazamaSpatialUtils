#' @keywords datagen
#' @importFrom rlang .data
#' @importFrom cleangeo clgeo_IsValid
#' @export
#' 
#' @title Convert Global Administrative Areas (GADM) Shapefile
#' 
#' @param nameOnly Logical specifying whether to only return the name without 
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01" 
#' versions of the file that are simplified to 5\%, 2\% and 1\%.#'
#' @param countryCode ISO-3166-1 alpha-2 country code
#' @param admLevel administrative level to be downloaded
#' 
#' @description Create a SpatialPolygonsDataFrame for Global Administrative Areas.
#' 
#' @details A Global Administrative Areas shapefile is downloaded and converted to a 
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file 
#' will be created in the spatial data directory which is set with 
#' \code{setSpatialDataDir()}.
#'
#' @note From the source documentation:
#' 
#' 
#' @return Name of the dataset being created.
#' @references \url{https://gadm.org/data.html}.

convertGADM <- function(
  nameOnly = FALSE,
  simplify = TRUE,
  countryCode = NULL, 
  admLevel = 0
) {

  # ----- Setup ----------------------------------------------------------------
  
  # Use package internal data directory
  dataDir <- getSpatialDataDir()
  
  # Specify the name of the dataset and file being created
  datasetName <- paste0('GADM_', countryCode, '_', admLevel)

  if (nameOnly) 
    return(datasetName)

  # Convert 2-character codes into ISO3
  ISO3 <- iso2ToIso3(countryCode)
  if ( is.na(ISO3) ) {
    stop('The countryCode parameter "', countryCode, '" is not an ISO-3166-1 alpha-2 country code.', call.=FALSE)
  }

  # ----- Get the data ---------------------------------------------------------
  
  # Build appropriate request URL
  url <- paste0('http://biogeo.ucdavis.edu/data/gadm2.8/rds/',
                ISO3, '_adm',
                admLevel, '.rds')

  # Get the data
  tempfile <- base::tempfile("spatial_data", fileext = ".rds")
  utils::download.file(url, tempfile)
  
  # ----- Convert to SPDF ------------------------------------------------------
  
  # Convert shapefile into SpatialPolygonsDataFrame
  SPDF <- readRDS(tempfile)
  base::file.remove(tempfile)

  # ----- Select useful columns and rename -------------------------------------
  
  if ( admLevel == 0 ) {

    # > dplyr::glimpse(SPDF@data)
    # Observations: 1
    # Variables: 68
    # OBJECTID      <int> 1
    # $ ID_0          <int> 244
    # $ ISO           <chr> "USA"
    # $ NAME_ENGLISH  <chr> "United States"
    # $ NAME_ISO      <chr> "UNITED STATES"
    # $ NAME_FAO      <chr> "United States of America"
    # $ NAME_LOCAL    <chr> "United States"
    # $ NAME_OBSOLETE <chr> ""
    # $ NAME_VARIANTS <chr> "United States of America|USA|US|U.S.|U.S.A."
    # $ NAME_NONLATIN <chr> ""
    # $ NAME_FRENCH   <chr> "États-Unis "
    # $ NAME_SPANISH  <chr> "Estados Unidos "
    # $ NAME_RUSSIAN  <chr> "Соединённые Штаты Америки"
    # $ NAME_ARABIC   <chr> "الولايات المتحدة "
    # $ NAME_CHINESE  <chr> "美国 "
    # $ WASPARTOF     <chr> ""
    # $ CONTAINS      <chr> ""
    # $ SOVEREIGN     <chr> "United States"
    # $ ISO2          <chr> "US"
    # $ WWW           <chr> ""
    # $ FIPS          <chr> "US"
    # $ ISON          <chr> "840"
    # $ VALIDFR       <chr> "19590821"
    # $ VALIDTO       <chr> "Present"
    # $ POP2000       <chr> "283230243"
    # $ SQKM          <chr> "9450720"
    # $ POPSQKM       <chr> "29.969170920310834"
    # $ UNREGION1     <chr> "Northern America"
    # $ UNREGION2     <chr> "Americas"
    # $ DEVELOPING    <chr> "2"
    # $ CIS           <chr> ""
    # $ Transition    <chr> ""
    # $ OECD          <chr> "1"
    # $ WBREGION      <chr> ""
    # $ WBINCOME      <chr> "High income: OECD"
    # $ WBDEBT        <chr> "Debt not classified"
    # $ WBOTHER       <chr> ""
    # $ CEEAC         <chr> ""
    # $ CEMAC         <chr> ""
    # $ CEPLG         <chr> ""
    # $ COMESA        <chr> ""
    # $ EAC           <chr> ""
    # $ ECOWAS        <chr> ""
    # $ IGAD          <chr> ""
    # $ IOC           <chr> ""
    # $ MRU           <chr> ""
    # $ SACU          <chr> ""
    # $ UEMOA         <chr> ""
    # $ UMA           <chr> ""
    # $ PALOP         <chr> ""
    # $ PARTA         <chr> ""
    # $ CACM          <chr> ""
    # $ EurAsEC       <chr> ""
    # $ Agadir        <chr> ""
    # $ SAARC         <chr> ""
    # $ ASEAN         <chr> ""
    # $ NAFTA         <chr> "1"
    # $ GCC           <chr> ""
    # $ CSN           <chr> ""
    # $ CARICOM       <chr> ""
    # $ EU            <chr> ""
    # $ CAN           <chr> ""
    # $  ACP           <chr> ""
    # $ Landlocked    <chr> ""
    # $ AOSIS         <chr> ""
    # $ SIDS          <chr> ""
    # $ Islands       <chr> ""
    # $ LDC           <chr> ""
  
    # Data Dictionary:
    #   OBJECTID -----> (drop)
    #   ID_0 ---------> (drop)
    #   ISO ----------> ISO3: ISO3 code
    #   NAME_ENGLISH -> countryName: English language name
    #   NAME_ISO -----> (drop)
    #   NAME_FAO -----> (drop)
    #   NAME_LOCAL ---> (drop)
    #   NAME_OBSOLETE > (drop)
    #   NAME_VARIANTS > (drop)
    #   NAME_NONLATIN > (drop)
    #   NAME_FRENCH  -> (drop)
    #   NAME_SPANISH -> (drop)
    #   NAME_RUSSIAN -> (drop)
    #   NAME_ARABIC --> (drop)
    #   NAME_CHINESE -> (drop)
    #   WASPARTOF ----> (drop)
    #   CONTAINS -----> (drop)
    #   SOVERIGN -----> (drop)
    #   ISO2 ---------> countryCode: 2-character country code
    #   WWW ----------> (drop)
    #   FIPS ---------> (drop)
    #   ISON ---------> (drop)
    #   VALIDFR ------> (drop)
    #   VALIDTO ------> (drop)
    #   POP2000 ------> (drop)
    #   SQKM ---------> (drop)
    #   POPSQKM ------> (drop)
    #   UNREGION1 ----> (drop)
    #   UNREGION2 ----> (drop)
    #   DEVELOPING ---> (drop)
    #   CIS ----------> (drop)
    #   Transition ---> (drop)
    #   OECD ---------> (drop)
    #   WBREGION -----> (drop)
    #   WBINCOME -----> (drop)
    #   WBDEBT -------> (drop)
    #   WBOTHER ------> (drop)
    #   CEEAC --------> (drop)
    #   CEMAC --------> (drop)
    #   CEPLG --------> (drop)
    #   COMESA -------> (drop)
    #   EAC ----------> (drop)
    #   ECOWAS -------> (drop)
    #   IGAD ---------> (drop)
    #   IOC ----------> (drop)
    #   MRU ----------> (drop)
    #   SACU ---------> (drop)
    #   UEMOA --------> (drop)
    #   UMA ----------> (drop)
    #   PALOP --------> (drop)
    #   PARTA --------> (drop)
    #   CACM ---------> (drop)
    #   EurAsEC ------> (drop)
    #   Agadir -------> (drop)
    #   SAARC --------> (drop)
    #   ASEAN --------> (drop)
    #   NAFTA --------> (drop)
    #   GCC ----------> (drop)
    #   CSN ----------> (drop)
    #   CARICOM ------> (drop)
    #   EU -----------> (drop)
    #   CAN ----------> (drop)
    #   ACP ----------> (drop)
    #   Landlocked ---> (drop)
    #   AOSIS --------> (drop)
    #   SIDS ---------> (drop)
    #   Islands ------> (drop)
    #   LDC ----------> (drop)

    # Create the new dataframe in a specific column order
    SPDF@data <- 
      dplyr::select(
        .data = SPDF@data,
        ISO3 = .data$ISO,
        countryCode = .data$ISO2,
        countryName = .data$NAME_ENGLISH
      )
    
  } else {

    # > dplyr::glimpse(SPDF@data)
    # Observations: 52
    # Variables: 13
    # $ OBJECTID  <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, …
    # $ ID_0      <int> 244, 244, 244, 244, 244, 244, 244, 244, 244, 244, 244, 244,…
    # $ ISO       <chr> "USA", "USA", "USA", "USA", "USA", "USA", "USA", "USA", "US…
    # $ NAME_0    <chr> "United States", "United States", "United States", "United …
    # $ ID_1      <int> 1, 2, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 1…
    # $ NAME_1    <chr> "Alabama", "Alaska", "Alaska", "Arizona", "Arkansas", "Cali…
    # $ HASC_1    <chr> "US.AL", "US.AK", "US.AK", "US.AZ", "US.AR", "US.CA", "US.C…
    # $ CCN_1     <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    # $ CCA_1     <chr> "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",…
    # $ TYPE_1    <chr> "State", "State", "State", "State", "State", "State", "Stat…
    # $ ENGTYPE_1 <chr> "State", "State", "State", "State", "State", "State", "Stat…
    # $ NL_NAME_1 <chr> "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",…
    # $ VARNAME_1 <chr> "AL|Ala.", "AK|Alaska", "AK|Alaska", "AZ|Ariz.", "AR|Ark.",…
    
    # Data Dictionary:
    #   OBJECTID -----> (drop)
    #   ISO ----------> ISO3: ISO3 code
    #   NAME_0 -------> countryName: English language name
    #   ID_1 ---------> (drop)
    #   NAME_1 -------> stateName: English language name
    #   HASC_1 -------> (drop)
    #   CCN_1 --------> (drop)
    #   TYPE_1 -------> (drop)
    #   ENGTYPE_1 ----> (drop)
    #   NL_NAME_1 ----> (drop)
    #   VARNAME_1 ----> (drop)
    #   ID_0 ---------> (drop)
    #   ID_0 ---------> (drop)
    
    # Add core metadata
    SPDF$countryCode <- iso3ToIso2(SPDF$ISO)
    SPDF$stateCode <- stringr::str_split_fixed(SPDF@data$HASC_1, '.', 4)[ , 4]

    # Create the new dataframe in a specific column order
    SPDF@data <- 
      dplyr::select(
        .data = SPDF@data,
        ISO3 = .data$ISO,
        countryCode = .data$countryCode,
        countryName = .data$NAME_0,
        stateName = .data$NAME_1,
        stateCode = .data$stateCode
      )

    if (admLevel >= 2) {
      SPDF$countyName <- SPDF$NAME_2
    }

  }

  # ----- Clean SPDF -----------------------------------------------------------
  
  # Group polygons with the same identifier (ISO3)
  SPDF <- organizePolygons(
    SPDF, 
    uniqueID = 'ISO3', 
    sumColumns = NULL
  )
  
  # Clean topology errors
  if ( !cleangeo::clgeo_IsValid(SPDF) ) {
    SPDF <- cleangeo::clgeo_Clean(SPDF, verbose = TRUE)
  }
  
  # ----- Name and save the data -----------------------------------------------
  
  # Assign a name and save the data
  message("Saving full resolution version...\n")
  assign(datasetName, SPDF)
  save(list = c(datasetName), file = paste0(dataDir, '/', datasetName, '.rda'))
  rm(list = datasetName)
  
  # ----- Simplify -------------------------------------------------------------
  
  if ( simplify ) {
    # Create new, simplified datsets: one with 5%, 2%, and one with 1% of the vertices of the original
    # NOTE:  This may take several minutes.
    message("Simplifying to 5%...\n")
    SPDF_05 <- rmapshaper::ms_simplify(SPDF, 0.05)
    SPDF_05@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    # Clean topology errors
    if ( !cleangeo::clgeo_IsValid(SPDF_05) ) {
      SPDF_05 <- cleangeo::clgeo_Clean(SPDF_05)
    }
    datasetName_05 <- paste0(datasetName, "_05")
    message("Saving 5% version...\n")
    assign(datasetName_05, SPDF_05)
    save(list = datasetName_05, file = paste0(dataDir,"/", datasetName_05, '.rda'))
    rm(list = c("SPDF_05",datasetName_05))
    
    message("Simplifying to 2%...\n")
    SPDF_02 <- rmapshaper::ms_simplify(SPDF, 0.02)
    SPDF_02@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    # Clean topology errors
    if ( !cleangeo::clgeo_IsValid(SPDF_02) ) {
      SPDF_02 <- cleangeo::clgeo_Clean(SPDF_02)
    }
    datasetName_02 <- paste0(datasetName, "_02")
    message("Saving 2% version...\n")
    assign(datasetName_02, SPDF_02)
    save(list = datasetName_02, file = paste0(dataDir,"/", datasetName_02, '.rda'))
    rm(list = c("SPDF_02",datasetName_02))
    
    message("Simplifying to 1%...\n")
    SPDF_01 <- rmapshaper::ms_simplify(SPDF, 0.01)
    SPDF_01@data$rmapshaperid <- NULL # Remove automatically generated "rmapshaperid" column
    # Clean topology errors
    if ( !cleangeo::clgeo_IsValid(SPDF_01) ) {
      SPDF_01 <- cleangeo::clgeo_Clean(SPDF_01)
    }
    datasetName_01 <- paste0(datasetName, "_01")
    message("Saving 1% version...\n")
    assign(datasetName_01, SPDF_01)
    save(list = datasetName_01, file = paste0(dataDir,"/", datasetName_01, '.rda'))
    rm(list = c("SPDF_01",datasetName_01))
  }
  
  # ----- Clean up and return --------------------------------------------------
  
  # Clean up
  # unlink(filePath, force = TRUE)
  # unlink(dsnPath, recursive = TRUE, force = TRUE)
  
  return(invisible(datasetName))
  
}