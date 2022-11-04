#' @keywords datagen
#' @export
#' @title Convert Wikipedia timezone table to dataframe
#' @description Returns a dataframe version of the Wikipedia timezone table with
#' the following columns:
#'
#' \itemize{
#'   \item{timezone -- Olson timezone}
#'   \item{UTC_offset -- hours between local timezone and UTC}
#'   \item{UTC_DST_offset -- hours between local timezone daylight savings and UTC}
#'   \item{countryCode -- ISO 3166-2 country code}
#'   \item{longitude -- longitude of the Olson timezone city}
#'   \item{latitude -- latitude of the Olson timezone city}
#'   \item{status -- either 'Canonical', 'Alias' or 'Deprecated'}
#'   \item{notes -- typically specifying the target of an 'Alias'}
#' }
#'
#' @details Older named timezones from the table which are linked to more modern
#' equivalents are not included in the returned dataframe.
#' @return Dataframe with 388 rows and 10 columns.
#' @references \url{https://en.wikipedia.org/wiki/List_of_tz_database_time_zones}
convertWikipediaTimezoneTable <- function() {

  url <- "https://en.wikipedia.org/wiki/List_of_tz_database_time_zones"

  # From the web page:
  #
  # This file contains a table with the following columns:
  # 1.  ISO 3166 2-character country code.  See the file `iso3166.tab'.
  # 2.  Latitude and longitude of the zone's principal location
  #     in ISO 6709 sign-degrees-minutes-seconds format,
  #     either +-DDMM+-DDDMM or +-DDMMSS+-DDDMMSS,
  #     first latitude (+ is north), then longitude (+ is east).
  # 3.  Zone name used in value of TZ environment variable.
  # 4.  Comments; present if and only if the country has multiple rows.

  # Assume the relevant list is the first table and parse that into a dataframe
  tzTable <-
    MazamaCoreUtils::html_getTable(url, header = TRUE, index = 1)

  # > dplyr::glimpse(tzTable, width = 75)
  # Rows: 597
  # Columns: 10
  # $ `Country code(s)`       <chr> "Country code(s)", "CI, BF, GH, GM, GN, I…
  # $ `TZ database name`      <chr> "TZ database name", "Africa/Abidjan", "Af…
  # $ `Area(s) covered`       <chr> "Area(s) covered", "", "", "", "", "", ""…
  # $ Type                    <chr> "Type", "Canonical", "Link†", "Link†", "C…
  # $ `UTC offset±hh:mm`      <chr> "STD", "+00:00", "+00:00", "+03:00", "+01…
  # $ `UTC offset±hh:mm`      <chr> "DST", "+00:00", "+00:00", "+03:00", "+01…
  # $ `Time zoneabbreviation` <chr> "STD", "GMT", "GMT", "EAT", "CET", "EAT",…
  # $ `Time zoneabbreviation` <chr> "DST", "GMT", "GMT", "EAT", "CET", "EAT",…
  # $ Sourcefile              <chr> "Sourcefile", "africa", "backward", "back…
  # $ Notes                   <chr> "Notes", "", "Link to Africa/Abidjan", "L…

  # Strip off the first row
  tzTable <- tzTable[-1,]

  # Rationalize naming:
  # * human readable full nouns with descriptive prefixes
  # * generally lowerCamelCase
  # with internal standards:
  # * timezone (Olson timezone)
  # * longitude (decimal degrees E)
  # * latitude (decimal degrees N)
  names(tzTable) <- c(
    'countryCodes', 'timezone', 'areaCovered', 'type',
    'UTC_STD_offset', 'UTC_DST_offset',
    'timezone_STD_abbreviation', 'timezone_DST_abbreviation',
    'sourceFile', 'notes'
  )

  # NOTE:  'countryCodes' may contain multiple codes. Here we pick the first one
  # NOTE:  and use it as the singular 'countryCode' that must be present in
  # NOTE:  all MazamaSpatialUtils datasets. (It appears that multiple codes are
  # NOTE:  listed in order of importance.)

  tzTable$countryCode <- stringr::str_sub(tzTable$countryCodes, 1, 2)

  # NOTE:  MazamaCoreUtils::html_getTable() has no argument to specify
  # NOTE:  na.strings so "NA" is converted to NA. Here we restore "NA".
  tzTable$countryCode[tzTable$timezone == 'Africa/Windhoek'] <- "NA"

  # Convert UTC_STD_offset "+HH:MM" to hours
  sign <- ifelse(stringr::str_sub(tzTable$UTC_STD_offset,1,1) == '+',1,-1)
  hour <- as.numeric(stringr::str_sub(tzTable$UTC_STD_offset,2,3))
  min <- as.numeric(stringr::str_sub(tzTable$UTC_STD_offset,5,6))
  tzTable$UTC_STD_offset <- sign * (hour + min/60)

  # Convert UTC_DST_offset "+HH:MM" to hours
  sign <- ifelse(stringr::str_sub(tzTable$UTC_DST_offset,1,1) == '+',1,-1)
  hour <- as.numeric(stringr::str_sub(tzTable$UTC_DST_offset,2,3))
  min <- as.numeric(stringr::str_sub(tzTable$UTC_DST_offset,5,6))
  tzTable$UTC_DST_offset <- sign * (hour + min/60)

  # Return desired columns in a sensible order
  keepColumns <- c(
    'timezone', 'countryCode', 'countryCodes',
    'timezone_STD_abbreviation', 'timezone_DST_abbreviation',
    'UTC_STD_offset', 'UTC_DST_offset',
    'notes'
  )

  return( tzTable[, keepColumns] )

}
