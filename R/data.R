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

#' @title Dataframe of US county codes
#' @format A dataframe with 3197 rows and 4 columns of data.
#' @description US_countyCodes The following columns for US states and territories:
#' \itemize{
#' \item{\code{stateCode} -- ISO 3166-2 alpha-2}
#' \item{\code{stateFIPS} -- 2-digit FIPS code}
#' \item{\code{countyName} -- English language county name}
#' \item{\code{countyFIPS} -- five-digit FIPS code (2-digit state and 3-digit
#' county combined to create a unique identifier)}
#' }
#'
#' This dataset was generated on 2022-11-04 by running:
#'
#' \preformatted{
#' library(MazamaSpatialUtils)
#' setSpatialDataDir("~/Data/Spatial_0.8")
#' loadSpatialData("USCensusCounties_02")
#'
#' US_countyCodes <-
#'   USCensusCounties_02 %>%
#'   dplyr::select(stateCode, stateFIPS, countyName, countyFIPS)
#'
#' US_countyCodes$geometry <- NULL
#'
#' save(US_countyCodes, file = "data/US_countyCodes.rda")
#' }
"US_countyCodes"


#' @title Simplified spatial dataset of world timezones.
#' @format A simple features data frame with 423 records and 9 columns of data.
#' @description This dataset is used by default in the \code{getTimezones()}
#' function and contains the following columns of data:
#' \itemize{
#' \item{\code{timezone} -- Olson timezone}
#' \item{\code{UTC_offset} -- offset from UTC (hours)}
#' \item{\code{UTC_DST_offset} -- offset from UTC during daylight savings (hours)}
#' \item{\code{countryCode} -- ISO 3166-1 alpha-2 country code}
#' \item{\code{longitude} -- longitude of the timezone polygon centroid}
#' \item{\code{latitude} -- longitude of the timezone polygon centroid}
#' \item{\code{status} -- one of 'Canonical', 'Alias' or 'Deprecated'}
#' \item{\code{notes} -- typically specifying the target of an 'Alias'}
#' \item{\code{polygonID} -- unique identifier (= \code{timezone})}
#' }
#'
#' This dataset was generated on 2022-11-03 by running:
#'
#' \preformatted{
#' library(MazamaSpatialUtils)
#' setSpatialDataDir("~/Data/Spatial_0.8")
#'
#' convertOSMTimezones()
#'
#' loadSpatialData("OSMTimezones_02")
#'
#' SimpleTimezones <- OSMTimezones_02
#' save(SimpleTimezones, file = "data/SimpleTimezones.rda")
#' }
"SimpleTimezones"

#' @title Simplified spatial dataset of EEZ/country combined boundaries.
#' @format A simple features data frame with 319 records and 5 columns of data.
#'
#' @description SimpleCountriesEEZ is a simplified world borders dataset with a
#' 200 mile coastal buffer corresponding to Exclusive Economic Zones, suitable for
#' quick spatial searches. This dataset is distributed with the package and is
#' used by default in \code{getCountry()}, \code{getCountryCode()} and
#' \code{getCountryName()}.
#'
#' @details This dataset is equivalent to EEZCountries but with fewer columns of data.
#' @seealso convertEEZCountries
#'
#' This dataset was generated on 2022-11-03 by running:
#'
#' \preformatted{
#' library(MazamaSpatialUtils)
#' setSpatialDataDir("~/Data/Spatial_0.8")
#'
#' convertEEZCountries()
#'
#' loadSpatialData("EEZCountries_05")
#'
#' SimpleCountriesEEZ <- EEZCountries_05[,c("countryCode", "countryName", "polygonID")]
#' save(SimpleCountriesEEZ, file = "data/SimpleCountriesEEZ.rda")
#' }
"SimpleCountriesEEZ"

#' @title Simplified spatial dataset of country boundaries.
#' @format A simple features data frame with 246 records and 7 columns of data.
#'
#' @description SimpleCountries is a simplified world borders dataset suitable
#' for global maps and quick spatial searches. This dataset is distributed with
#' the package and is can be used with \code{getCountry()},
#' \code{getCountryCode()} and \code{getCountryName()} when restricting searches
#' to land-based locations.
#'
#' @details This dataset is equivalent to TMWorldBordersSimple but with fewer columns of data.
#' @seealso convertTMWorldBordersSimple
#'
#' This dataset was generated on 2022-11-04 by running:
#'
#' \preformatted{
#' library(MazamaSpatialUtils)
#' setSpatialDataDir("~/Data/Spatial_0.8")
#'
#' convertTMWorldBorders()
#'
#' loadSpatialData("NaturalEarthAdm0_05")
#'
#' columnNames <- c("countryCode", "countryName", "ISO3", "FIPS",
#'                  "UN_region", "polygonID")
#' SimpleCountries <- NaturalEarthAdm0_05[, columnNames]
#' save(SimpleCountries, file = "data/SimpleCountries.rda")
#' }
"SimpleCountries"

