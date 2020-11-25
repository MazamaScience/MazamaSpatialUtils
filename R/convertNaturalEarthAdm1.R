#' @keywords datagen
#' @importFrom rlang .data
#' @export
#'
#' @title Convert Level 1 (State) Borders Shapefile
#'
#' @param nameOnly Logical specifying whether to only return the name without
#' creating the file.
#' @param simplify Logical specifying whether to create "_05", _02" and "_01"
#' versions of the file that are simplified to 5\%, 2\% and 1\%.
#'
#' @description Returns a SpatialPolygonsDataFrame for 1st level administrative divisions
#'
#' @details A state border shapefile is downloaded and converted to a
#' SpatialPolygonsDataFrame with additional columns of data. The resulting file
#' will be created in the spatial data directory which is set with
#' \code{setSpatialDataDir()}.
#'
#' Within the \pkg{MazamaSpatialUtils} package the phrase 'state' refers to
#' administrative divisions beneath the level of the country or nation. This
#' makes sense in the United 'States'. In other countries this level is known as
#' 'province', 'territory' or some other term.
#'
#' @return Name of the dataset being created.
#'
#' @references \url{http://www.naturalearthdata.com/downloads/}
#' @references \url{http://www.statoids.com/ihasc.html}
#' @seealso setSpatialDataDir
#' @seealso getVariable
convertNaturalEarthAdm1 <- function(
  nameOnly = FALSE,
  simplify = TRUE
) {

  # ----- Setup ----------------------------------------------------------------

  # Use package internal data directory
  dataDir <- getSpatialDataDir()

  # Specify the name of the dataset and file being created
  datasetName <- 'NaturalEarthAdm1'

  if (nameOnly)
    return(datasetName)

  # ----- Get the data ---------------------------------------------------------

  # Build appropriate request URL
  url <- 'http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_1_states_provinces.zip'

  filePath <- file.path(dataDir, basename(url))
  utils::download.file(url, filePath)
  # NOTE:  This zip file has no directory so extra subdirectory needs to be created
  utils::unzip(filePath, exdir = file.path(dataDir, 'adm'))

  # ----- Convert to SPDF ------------------------------------------------------

  # Convert shapefile into SpatialPolygonsDataFrame
  # NOTE:  The 'adm' directory has been created
  dsnPath <- file.path(dataDir, 'adm')
  shpName <- 'ne_10m_admin_1_states_provinces'
  SPDF <- convertLayer(
    dsn = dsnPath,
    layerName = shpName,
    encoding = 'UTF-8'
  )

  # ----- Select useful columns and rename -------------------------------------

  # > dplyr::glimpse(SPDF@data)
  # Observations: 4,594
  # Variables: 83
  # $ featurecla <chr> "Admin-1 scale rank", "Admin-1 scale rank", "Admin-1 scale…
  # $ scalerank  <int> 3, 6, 3, 2, 2, 6, 3, 4, 4, 3, 4, 3, 4, 3, 3, 10, 2, 2, 2, …
  # $ adm1_code  <chr> "ARG-1309", "URY-8", "PAK-1114", "IND-3264", "IDN-1185", "…
  # $ diss_me    <int> 1309, 8, 1114, 3264, 1185, 1186, 2694, 1936, 1937, 2693, 1…
  # $ iso_3166_2 <chr> "AR-E", "UY-PA", "PK-SD", "IN-GJ", "ID-KI", "MY-12", "CL-A…
  # $ wikipedia  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
  # $ iso_a2     <chr> "AR", "UY", "PK", "IN", "ID", "MY", "CL", "BO", "BO", "CL"…
  # $ adm0_sr    <int> 1, 1, 1, 1, 5, 5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1…
  # $ name       <chr> "Entre Ríos", "Paysandú", "Sind", "Gujarat", "Kalimantan T…
  # $ name_alt   <chr> "Entre-Rios", NA, "Sindh", NA, "Kaltim", "North Borneo", N…
  # $ name_local <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
  # $ type       <chr> "Provincia", "Departamento", "Province", NA, "Propinsi", "…
  # $ type_en    <chr> "Province", "Department", "Province", NA, "Province", "Sta…
  # $ code_local <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
  # $ code_hasc  <chr> "AR.ER", "UY.PA", "PK.SD", "IN.GJ", "ID.KI", "MY.SA", "CL.…
  # $ note       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
  # $ hasc_maybe <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "BO.OR|BOL-POT", N…
  # $ region     <chr> NA, NA, NA, "West", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
  # $ region_cod <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
  # $ provnum_ne <int> 10, 19, 2, 20064, 15, 1, 0, 8, 7, 0, 1, 20006, 8, 22, 5, 0…
  # $ gadm_level <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1…
  # $ check_me   <int> 20, 0, 20, 20, 20, 20, 20, 0, 0, 20, 10, 20, 10, 20, 20, 2…
  # $ datarank   <int> 3, 8, 2, 1, 1, 6, 3, 8, 6, 3, 6, 3, 5, 3, 3, 10, 1, 1, 1, …
  # $ abbrev     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
  # $ postal     <chr> "ER", "PA", "SD", NA, "KI", "SA", NA, "LP", "OR", "TA", "P…
  # $ area_sqkm  <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
  # $ sameascity <int> -99, -99, -99, -99, -99, -99, 7, 6, 6, -99, -99, 6, 7, 6, …
  # $ labelrank  <int> 3, 6, 3, 2, 2, 6, 7, 6, 6, 3, 4, 6, 7, 6, 3, 20, 2, 2, 2, …
  # $ name_len   <int> 10, 8, 4, 7, 16, 5, 18, 6, 5, 8, 6, 11, 5, 5, 5, 8, 17, 8,…
  # $ mapcolor9  <int> 3, 2, 3, 2, 6, 3, 5, 2, 2, 5, 2, 5, 4, 3, 3, 6, 2, 4, 4, 5…
  # $ mapcolor13 <int> 13, 10, 11, 2, 11, 6, 9, 3, 3, 9, 3, 9, 11, 13, 13, 3, 2, …
  # $ fips       <chr> "AR08", "UY11", "PK05", "IN32", "ID14", "MY16", NA, "BL04"…
  # $ fips_alt   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "BL05", NA, "PE18"…
  # $ woe_id     <int> 2344682, 2347650, 2346499, 2345743, 2345723, 2346310, 5604…
  # $ woe_label  <chr> "Entre Rios, AR, Argentina", "PaysandÃº, UY, Uruguay", "Si…
  # $ woe_name   <chr> "Entre Ríos", "Paysandú", "Sind", "Gujarat", "Kalimantan T…
  # $ latitude   <dbl> -32.02750, -32.09330, 26.37340, 22.75010, 1.28915, 5.31115…
  # $ longitude  <dbl> -59.2824, -57.2240, 68.8685, 71.3013, 116.3540, 117.0950, …
  # $ sov_a3     <chr> "ARG", "URY", "PAK", "IND", "IDN", "MYS", "CHL", "BOL", "B…
  # $ adm0_a3    <chr> "ARG", "URY", "PAK", "IND", "IDN", "MYS", "CHL", "BOL", "B…
  # $ adm0_label <int> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 5, 2, 2, 2…
  # $ admin      <chr> "Argentina", "Uruguay", "Pakistan", "India", "Indonesia", …
  # $ geonunit   <chr> "Argentina", "Uruguay", "Pakistan", "India", "Indonesia", …
  # $ gu_a3      <chr> "ARG", "URY", "PAK", "IND", "IDN", "MYS", "CHL", "BOL", "B…
  # $ gn_id      <int> 3434137, 3441242, 1164807, 1270770, 1641897, 1733039, 6693…
  # $ gn_name    <chr> "Provincia de Entre Rios", "Departamento de Paysandu", "Si…
  # $ gns_id     <int> -988655, -908097, -2774813, -2096768, -2680740, -2405166, …
  # $ gns_name   <chr> "Entre Rios", "Paysandu, Departamento de", "Sindh", "Gujar…
  # $ gn_level   <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1…
  # $ gn_region  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
  # $ gn_a1_code <chr> "AR.08", "UY.11", "PK.05", "IN.09", "ID.14", "MY.16", "CL.…
  # $ region_sub <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
  # $ sub_code   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
  # $ gns_level  <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1…
  # $ gns_lang   <chr> "khm", "fra", "rus", "nld", "ind", "fil", "ara", "kor", "k…
  # $ gns_adm1   <chr> "AR08", "UY11", "PK05", "IN09", "ID14", "MY16", "CI16", "B…
  # $ gns_region <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
  # $ min_label  <dbl> 6.0, 8.0, 5.0, 4.6, 5.0, 7.0, 6.0, 6.6, 6.6, 6.0, 6.6, 6.0…
  # $ max_label  <dbl> 11.0, 11.0, 10.5, 10.1, 10.1, 11.0, 11.0, 11.0, 11.0, 11.0…
  # $ min_zoom   <dbl> 6.0, 8.0, 5.0, 4.6, 4.6, 7.0, 6.0, 6.6, 6.6, 6.0, 6.6, 6.0…
  # $ wikidataid <chr> "Q44762", "Q16576", "Q37211", "Q1061", "Q3899", "Q179029",…
  # $ name_ar    <chr> "إنتري ريوس", "إدارة بايساندو", "السند", "غوجارات", "كالمن…
  # $ name_bn    <chr> "এন্ত্রে রিও প্রদেশ", "পেসান্ডো বিভাগ", "সিন্ধু প্রদেশ", "…
  # $ name_de    <chr> "Entre Ríos", "Departamento Paysandú", "Sindh", "Gujarat",…
  # $ name_en    <chr> "Entre Ríos Province", "Paysandú Department", "Sindh", "Gu…
  # $ name_es    <chr> "Entre Ríos", "Departamento de Paysandú", "Sindh", "Guyara…
  # $ name_fr    <chr> "Entre Ríos", "Paysandú", "Sind", "Gujarat", "Kalimantan o…
  # $ name_el    <chr> "Έντρε Ρίος", "Παϊσαντού", "Σιντ", "Γκουτζαράτ", "Ανατολικ…
  # $ name_hi    <chr> "एन्ट्रे रियोस", "पयसंदु विभाग", "सिंध", "गुजरात", "पूर्व …
  # $ name_hu    <chr> "Entre Ríos tartomány", "Paysandú megye", "Szindh", "Gudzs…
  # $ name_id    <chr> "Provinsi Entre Ríos", "Departemen Paysandú", "Sindh", "Gu…
  # $ name_it    <chr> "provincia di Entre Ríos", "dipartimento di Paysandú", "Si…
  # $ name_ja    <chr> "エントレ・リオス州", "パイサンドゥ県", "シンド州", "グジャラート州", "東カリマンタン州", "サバ…
  # $ name_ko    <chr> "엔트레리오스 주", "파이산두 주", "신드 주", "구자라트 주", "칼리만탄티무르 주", "사바 주…
  # $ name_nl    <chr> "Entre Ríos", "Paysandú", "Sindh", "Gujarat", "Oost-Kalima…
  # $ name_pl    <chr> "Entre Ríos", "Paysandú", "Sindh", "Gudźarat", "Borneo Wsc…
  # $ name_pt    <chr> "Entre Ríos", "Paysandú", "Sind", "Gujarate", "Kalimantan …
  # $ name_ru    <chr> "Энтре-Риос", "Пайсанду", "Синд", "Гуджарат", "Восточный К…
  # $ name_sv    <chr> "Entre Ríos", "Paysandú", "Sindh", "Gujarat", "Kalimantan …
  # $ name_tr    <chr> "Entre Ríos eyaleti", "Paysandu Departmanı", "Sind Eyaleti…
  # $ name_vi    <chr> "Entre Ríos", "Paysandú", "Sindh", "Gujarat", "Đông Kalima…
  # $ name_zh    <chr> "恩特雷里奥斯省", "派桑杜省", "信德省", "古吉拉特邦", "東加里曼丹省", "沙巴", "阿里卡和帕里…
  # $ ne_id      <chr> "1159309789", "1159307733", "1159309351", "1159314179", "1…

  # Data Dictionary:
  #   featurecla --> (drop)
  #   scalerank ---> (drop)
  #   adm1_code ---> adm1_code: Unique identifier
  #   diss_me -----> (drop)
  #   iso_3166_2 --> (drop)
  #   wikipedia ---> (drop)
  #   iso_a2 ------> countryCode: 2-character country code
  #   adm0_sr -----> (drop)
  #   name --------> stateName: Native language name
  #   name_alt ----> (drop)
  #   name_local --> (drop)
  #   type --------> (drop)
  #   type_en -----> (drop)
  #   code_local --> (drop)
  #   code_hasc ---> code_hasc: HASC code
  #   note --------> (drop)
  #   hasc_maybe --> (drop)
  #   region ------> (drop)
  #   region_cod --> (drop)
  #   provnum_ne --> (drop)
  #   gadm_level --> (drop)
  #   check_me ----> (drop)
  #   datarank ----> (drop)
  #   abbrev ------> (drop)
  #   postal ------> postal: country FIPS code
  #   area_sqkm ---> area_sqkm: area in square kilometers
  #   sameascity --> (drop)
  #   labelrank ---> (drop)
  #   name_len ----> (drop)
  #   mapcolor9 ---> (drop)
  #   mapcolor13 --> (drop)
  #   fips --------> stateFIPS: FIPS code
  #   fips_alt ----> (drop)
  #   woe_id ------> (drop)
  #   woe_label ---> (drop)
  #   woe_name ----> (drop)
  #   latitude ----> latitude: latitude coordinate
  #   longitude ---> longitude: longitude coordinate
  #   sov_a3 ------> (drop)
  #   adm0_a3 -----> (drop)
  #   adm0_label --> (drop)
  #   admin -------> (drop)
  #   geounit -----> (drop)
  #   gu_a3 -------> (drop)
  #   gn_id -------> (drop)
  #   gn_name -----> (drop)
  #   gns_id ------> (drop)
  #   gns_name ----> (drop)
  #   gn_level ----> (drop)
  #   gn_region ---> (drop)
  #   gn_a1_code --> (drop)
  #   region_sub --> (drop)
  #   sub_code ----> (drop)
  #   gns_level ---> (drop)
  #   gns_lang ----> gns_lang: ISO 639-3 language identifier
  #   gns_adm1 ----> gns_adm1: adm1 identifier
  #   gns_reigon --> (drop)
  #   min_label ---> (drop)
  #   max_label ---> (drop)
  #   min_zoom ----> (drop)
  #   wikidataid --> (drop)
  #   name_ar -----> (drop)
  #   name_bn -----> (drop)
  #   name_de -----> (drop)
  #   name_en -----> (drop)
  #   name_es -----> (drop)
  #   name_fr -----> (drop)
  #   name_el -----> (drop)
  #   name_hi -----> (drop)
  #   name_hu -----> (drop)
  #   name_id -----> (drop)
  #   name_it -----> (drop)
  #   name_ja -----> (drop)
  #   name_ko -----> (drop)
  #   name_nl -----> (drop)
  #   name_pl -----> (drop)
  #   name_pt -----> (drop)
  #   name_ru -----> (drop)
  #   name_sv -----> (drop)
  #   name_tr -----> (drop)
  #   name_vi -----> (drop)
  #   name_zh -----> (drop)
  #   nd_id -------> (drop)

  # NOTE:  Subset to filter out exceptional areas (islands and disputed
  # NOTE:  territories) marked with '~'

  goodAreas <- stringr::str_subset(SPDF@data$code_hasc, "~", negate = TRUE)

  SPDF <- subset(SPDF, SPDF@data$code_hasc %in% goodAreas)

  # Add the core identifiers to the SpatialPolygonsDataFrame
  SPDF$stateCode <-
    SPDF@data$code_hasc %>%
    stringr::str_trim() %>%
    stringr::str_sub(4,-1)
  SPDF$countryName <- MazamaSpatialUtils::codeToCountry(SPDF@data$iso_a2)
  SPDF$stateName <- SPDF@data$name
  SPDF$stateFIPS <- SPDF@data$fips

  # Create the new dataframe in a specific column order
  SPDF@data <-
    dplyr::select(
      .data = SPDF@data,
      countryCode = .data$iso_a2,
      countryName = .data$countryName,
      stateCode = .data$stateCode,
      stateName = .data$name,
      stateFIPS = .data$stateFIPS,
      latitude = .data$latitude,
      longitude = .data$longitude,
      area_sqkm = .data$area_sqkm,
      postal = .data$postal,
      adm1_code = .data$adm1_code,
      code_hasc = .data$code_hasc,
      gns_lang = .data$gns_lang,
      gns_adm1 = .data$gns_adm1
    )

  # ----- Clean SPDF -----------------------------------------------------------

  # Group polygons with the same identifier (adm1_code)
  SPDF <- organizePolygons(
    SPDF,
    uniqueID = 'adm1_code',
    sumColumns = 'area_sqkm'
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
  unlink(filePath, force = TRUE)
  unlink(dsnPath, recursive = TRUE, force = TRUE)

  return(invisible(datasetName))

}
