#'
#' @name US_stateConversion
#' @aliases US_stateCodeToName US_stateCodeToFIPS
#' @aliases US_stateFIPSToName US_stateCodeToCode
#' @aliases US_stateNameToCode US_stateNameToFIPS
#' 
#' @title Conversion functions for US state names, codes and FIPS codes.
#' 
#' @param stateName Vector of English language state names.
#' @param stateCode Vector of ISO 3166-2 alpha-2 codes.
#' @param stateFIPS Vector of two-digit FIPS codes.
#' 
#' @return A vector of US state names or codes.
#' 
#' @description Converts a vector of US state names or codes from one system to
#' another reuturning \code{NA} where no match is found.
#' 
#' @examples 
#' library(MazamaSpatialUtils)
#' 
#' US_stateNameToCode("Washington")
#' US_stateNameToFIPS("Washington")
#' 
#' postalCodes <- sample(US_stateCodes$stateCode, 30)
#' 
#' data.frame(
#'   name = US_stateCodeToName(postalCodes),
#'   code = postalCodes,
#'   FIPS = US_stateCodeToFIPS(postalCodes)
#' )
#' 
NULL

#' @rdname US_stateConversion
#' @export
US_stateCodeToName <- function(
  stateCode = NULL
) {
  values <- US_stateCodes$stateName
  names(values) <- toupper(US_stateCodes$stateCode)
  return( as.character(values[toupper(stateCode)]) )
}

#' @rdname US_stateConversion
#' @export
US_stateCodeToFIPS <- function(
  stateCode = NULL
) {
  values <- US_stateCodes$stateFIPS
  names(values) <- toupper(US_stateCodes$stateCode)
  return( as.character(values[toupper(stateCode)]) )
}

#' @rdname US_stateConversion
#' @export
US_stateFIPSToName <- function(
  stateFIPS = NULL
) {
  values <- US_stateCodes$stateName
  names(values) <- toupper(US_stateCodes$stateFIPS)
  return( as.character(values[toupper(stateFIPS)]) )
}

#' @rdname US_stateConversion
#' @export
US_stateFIPSToCode <- function(
  stateFIPS = NULL
) {
  values <- US_stateCodes$stateCode
  names(values) <- toupper(US_stateCodes$stateFIPS)
  return( as.character(values[toupper(stateFIPS)]) )
}

#' @rdname US_stateConversion
#' @export
US_stateNameToCode <- function(
  stateName = NULL
) {
  values <- US_stateCodes$stateCode
  names(values) <- toupper(US_stateCodes$stateName)
  return( as.character(values[toupper(stateName)]) )
}

#' @rdname US_stateConversion
#' @export
US_stateNameToFIPS <- function(
  stateName = NULL
) {
  values <- US_stateCodes$stateFIPS
  names(values) <- toupper(US_stateCodes$stateName)
  return( as.character(values[toupper(stateName)]) )
}

