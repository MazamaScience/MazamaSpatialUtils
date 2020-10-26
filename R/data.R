#' @export
#' @docType data
#' @name US_stateCodes
#' @title Dataframe of US state codes
#' @format A dataframe with 52 rows and 3 columns of data.
#' @description US_stateCodes the following columns for US states and territories:
#' \itemize{
#' \item{\code{stateName} -- English language state name}
#' \item{\code{stateCode} -- ISO 3166-2 alpha-2}
#' \item{\code{stateFIPS} -- two-digit FIPS code}
#' }
#' 
#' This dataset was generated on 2020-06-11 by running:
#'
#' \preformatted{
# url <- "https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code"
# US_stateCodes <- url %>%
#   xml2::read_html() %>%
#   rvest::html_nodes("table") %>%
#   `[[`(1) %>%
#   rvest::html_table() %>%
#   dplyr::mutate(
#     stateName = Name,
#     stateCode = `Alpha code`,
#     stateFIPS = `Numeric code`
#   ) %>%
#   dplyr::select(c("stateName", "stateCode", "stateFIPS")) %>%
#   dplyr::filter(stateCode != "") %>%
#   dplyr::filter(as.numeric(stateFIPS) %in% c(1:56,72)) # 50 states + DC + Puerto Rico
# 
# # Convert stateCode from numeric to "2-char"
# US_stateCodes$stateFIPS <- sprintf("%02d", US_stateCodes$stateFIPS)
# 
# dump("US_stateCodes", file = "")
#' }
US_stateCodes <-
  structure(list(
    stateName = c("Alabama", "Alaska", "Arizona", 
                  "Arkansas", "California", "Colorado", "Connecticut", "Delaware", 
                  "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", 
                  "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
                  "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
                  "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
                  "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
                  "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Puerto Rico", 
                  "Rhode Island", "South Carolina", "South Dakota", "Tennessee", 
                  "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", 
                  "Wisconsin", "Wyoming"), 
    stateCode = c("AL", "AK", "AZ", "AR", 
                  "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", 
                  "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", 
                  "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", 
                  "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", 
                  "WA", "WV", "WI", "WY"), 
    stateFIPS = c("01", "02", "04", "05", 
                  "06", "08", "09", "10", "11", "12", "13", "15", "16", "17", "18", 
                  "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", 
                  "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", 
                  "41", "42", "72", "44", "45", "46", "47", "48", "49", "50", "51", 
                  "53", "54", "55", "56")), 
    row.names = c(NA, -52L), 
    class = "data.frame")

#' @export
#' @docType data
#' @name CONUS
#' @title CONUS state codes
#' @format A vector with 49 elements
#' @description
#' State codes for the 48 contiguous states +DC that make up the CONtinental US.

CONUS <- c(
       "AL","AZ","AR","CA","CO","CT","DE","FL","GA",
       "ID","IL","IN","IA","KS","KY","LA","ME","MD",
  "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
  "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
  "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY",
  "DC"     
)

#' @export
#' @docType data
#' @name US_52
#' @title US state codes
#' @format A vector with 52 elements
#' @description
#' State codes for the 50 states +DC +PR (Puerto Rico).

US_52 <- c(
  "AK","AL","AZ","AR","CA","CO","CT","DE","FL","GA",
  "HI","ID","IL","IN","IA","KS","KY","LA","ME","MD",
  "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
  "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
  "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY",
  "DC","PR"
)

# ===== Data in the data/ directory ============================================

#' @title Dataframe of US state codes
#' @format A dataframe with 3196 rows and 4 columns of data.
#' @description US_countyCodes The following columns for US states and territories:
#' \itemize{
#' \item{\code{stateCode} -- ISO 3166-2 alpha-2}
#' \item{\code{stateFIPS} -- 2-digit FIPS code}
#' \item{\code{countyName} -- English language county name}
#' \item{\code{countyFIPS} -- five-digit FIPS code (2-digit state and 3-digit 
#' county combined to create a unique identifier)}
#' }
#' 
#' This dataset was generated on 2020-10-26 by running:
#'
#' \preformatted{
#' library(MazamaSpatialUtils)
#' setSpatialDataDir("~/Data/Spatial")
#' loadSpatialData("USCensusCounties_02")
#' 
#' US_countyCodes <- 
#'   USCensusCounties_02@data %>%
#'   dplyr::select(stateCode, stateFIPS, countyName, countyFIPS)
#'   
#' save(US_countyCodes, file = "data/US_countyCodes.rda")
#' }
"US_countyCodes"
