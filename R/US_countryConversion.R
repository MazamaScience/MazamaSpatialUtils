#'
#' @name countryConversion
#' @aliases countryCodeToName countryCodeToFIPS
#' @aliases countryFIPSToName countryFIPSToCode
#' @aliases countryNameToCode countryNameToFIPS
#'
#' @title Conversion functions for country names, codes and FIPS codes.
#'
#' @param countryName Vector of English language country names.
#' @param countryCode Vector of ISO 3166-1 alpha-2 codes.
#' @param countryFIPS Vector of two-character FIPS codes.
#'
#' @return A vector of country names or codes.
#'
#' @description Converts a vector of country names or codes from one system to
#' another returning \code{NA} where no match is found.
#'
#' @examples
#' library(MazamaSpatialUtils)
#'
#' # FIPS codes are different!
#'
#' countryNameToCode("Germany")
#' countryNameToFIPS("Germany")
#'
#' countryCodeToName("CH")
#' countryFIPSToName("CH")
#'
#' countryCodes <- sample(SimpleCountries$countryCode, 30)
#'
#' data.frame(
#'   name = countryCodeToName(countryCodes),
#'   code = countryCodes,
#'   FIPS = countryCodeToFIPS(countryCodes)
#' )
#'
NULL

#' @rdname countryConversion
#' @export
countryCodeToName <- function(
  countryCode = NULL
) {
  countryName <- countrycode::countrycode(
    countryCode, "iso2c", "country.name",
    custom_match = c("AN" = "Netherlands Antilles") # custom match for Netherlands Antilles
  )
  return(countryName)
}

#' @rdname countryConversion
#' @export
countryCodeToFIPS <- function(
  countryCode = NULL
) {
  values <- MazamaSpatialUtils::SimpleCountries$FIPS
  names(values) <- toupper(MazamaSpatialUtils::SimpleCountries$countryCode)
  return( as.character(values[toupper(countryCode)]) )
}

#' @rdname countryConversion
#' @export
countryFIPSToName <- function(
  countryFIPS = NULL
) {
  countryName <- countryFIPS %>% countryFIPSToCode() %>% countryCodeToName()
  return(countryName)
}

#' @rdname countryConversion
#' @export
countryFIPSToCode <- function(
  countryFIPS = NULL
) {
  values <- MazamaSpatialUtils::SimpleCountries$countryCode
  names(values) <- toupper(MazamaSpatialUtils::SimpleCountries$FIPS)
  return( as.character(values[toupper(countryFIPS)]) )
}

#' @rdname countryConversion
#' @export
countryNameToCode <- function(
  countryName = NULL
) {
  countryCode <- countrycode::countrycode(
    countryName, "country.name", "iso2c",
    custom_match = c("Netherlands Antilles" = "AN") # custom match for Netherlands Antilles
  )
  return(countryCode)
}

#' @rdname countryConversion
#' @export
countryNameToFIPS <- function(
  countryName = NULL
) {
  countryFIPS <- countryName %>% countryNameToCode() %>% countryCodeToFIPS()
  return(countryFIPS)
}

