#'
#' @name US_countyConversion
#' @aliases US_countyFIPSToName US_countyNameToFIPS
#'
#' @title Conversion functions for US county names and FIPS codes.
#'
#' @param state Vector of state codes, names or FIPS codes. Values will be
#' evaluated to determine the type of input.
#' @param countyName Vector of English language county names.
#' @param countyFIPS Vector of two-digit FIPS codes.
#'
#' @return A vector of US county names or FIPS codes.
#'
#' @description Converts a vector of US county names or FIPS codes from one
#' system to another returning \code{NA} where no match is found.
#'
#' @examples
#' library(MazamaSpatialUtils)
#'
#' US_countyNameToFIPS("Washington", "King")
#'
#' # If a single state is provided, it will be recycled
#' US_countyNameToFIPS("Washington", c("King", "Okanogan"))
#'
#' # Normally, equal length vectors are provided
#' US_countyNameToFIPS(c("WA", "WA"), c("King", "Okanogan"))
#'
#' # You cannot mix codes!
#' US_countyNameToFIPS(c("WA", "Washington"), c("King", "Okanogan"))
#'
#' # No 'Okanogan' county in Texas
#' US_countyNameToFIPS(c("WA", "TX"), c("King", "Okanogan"))
#'
#' # But there is a 'King' county in Texas
#' US_countyNameToFIPS(c("TX", "WA"), c("King", "Okanogan"))
#' US_countyNameToFIPS(c("TX", "WA"), c("King", "King"))
#'
#' # The US_countyFIPSToName() function is included for symmetry but a
#' # more typical usage of a 5-digit county FIPS would be to extract it from
#' # the US_countyCodes package dataset:
#'
#' US_countyCodes %>% dplyr::filter(countyFIPS == 53033)
#'
NULL

#' @rdname US_countyConversion
#' @export
US_countyNameToFIPS <- function(
  state = NULL,
  countyName = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  state <- as.character(state)

  if ( length(state) == 1 )
    state <- rep(state, length.out = length(countyName) )

  if ( length(state) != length(countyName) )
    stop("Parameter 'state' must have the same length as 'countyName' or be of length 1")

  # ----- Get stateFIPS --------------------------------------------------------

  if ( state[1] %in% US_stateCodes$stateCode ) {
    stateFIPS <- US_stateCodeToFIPS(state)
  } else if ( tolower(state[1]) %in%  tolower(US_stateCodes$stateName) ) {
    stateFIPS <- US_stateNameToFIPS(state)
  } else if ( state[1] %in% US_stateCodes$stateFIPS ) {
    stateFIPS <- state
  } else {
    stop(sprintf("state = \"%s\" is not recognized.", state))
  }

  # ----- Find countyFIPS ------------------------------------------------------

  state_county_name <-
    paste0(stateFIPS, "_", countyName) %>%
    tolower()

  US_state_county_name <-
    paste0(
      MazamaSpatialUtils::US_countyCodes$stateFIPS,
      "_",
      MazamaSpatialUtils::US_countyCodes$countyName
    ) %>%
    tolower()

  indices <- match(state_county_name, US_state_county_name)

  countyFIPS <-
    MazamaSpatialUtils::US_countyCodes$countyFIPS[indices]

  # ----- Return ---------------------------------------------------------------

  return(countyFIPS)

}

#' @rdname US_countyConversion
#' @export
US_countyFIPSToName <- function(
  state = NULL,
  countyFIPS = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  state <- as.character(state)

  if ( length(state) == 1 )
    state <- rep(state, length.out = length(countyFIPS) )

  if ( length(state) != length(countyFIPS) )
    stop("Parameter 'state' must have the same length as 'countyFIPS' or be of length 1")

  # ----- Get stateFIPS --------------------------------------------------------

  if ( state[1] %in% US_stateCodes$stateCode ) {
    stateFIPS <- US_stateCodeToFIPS(state)
  } else if ( tolower(state[1]) %in%  tolower(US_stateCodes$stateName) ) {
    stateFIPS <- US_stateNameToFIPS(state)
  } else if ( state[1] %in% US_stateCodes$stateFIPS ) {
    stateFIPS <- state
  } else {
    stop(sprintf("state = \"%s\" is not recognized.", state))
  }

  # ----- Find countyFIPS ------------------------------------------------------

  state_county_FIPS <-
    paste0(stateFIPS, "_", countyFIPS) %>%
    tolower()

  US_state_county_FIPS <-
    paste0(
      MazamaSpatialUtils::US_countyCodes$stateFIPS,
      "_",
      MazamaSpatialUtils::US_countyCodes$countyFIPS
    ) %>%
    tolower()

  indices <- match(state_county_FIPS, US_state_county_FIPS)

  countyName <-
    MazamaSpatialUtils::US_countyCodes$countyName[indices]

  # ----- Return ---------------------------------------------------------------

  return(countyName)

}

