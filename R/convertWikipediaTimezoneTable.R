#' @keywords datagen
#' @export
#' @title Convert Wikipedia Timezone Table to Dataframe
#' @description Returns a dataframe version of the Wikipedia timezone table with the following columns:
#' \itemize{
#'   \item{timezone -- Olson timezone}
#'   \item{UTC_offset -- hours between local timezone and UTC}
#'   \item{UTC_DST_offset -- hours between local timezone daylight savings and UTC}
#'   \item{countryCode -- ISO 3166-2 country code}
#'   \item{longitude -- longitude of the Olson timezone city}
#'   \item{latitude -- latitude of the Olson timezone city}
#' }
#' @details Older named timezones from the table which are linked to more modern 
#' equivalents are not included in the returned dataframe.
#' @return Dataframe with 399 rows and 6 columns.
#' @references \url{http://en.wikipedia.org/wiki/List_of_tz_database_time_zones}
convertWikipediaTimezoneTable <- function() {
  
  url <- "http://en.wikipedia.org/wiki/List_of_tz_database_time_zones"
  
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
  
  # Get the raw html from the url
  wikiDoc <- xml2::read_html(url)

  # Get a list of tables in the documentSaraj
  tables <- rvest::html_nodes(wikiDoc, "table")
  
  # Assume the relevant list is the first table and parse that into a dataframe
  tzTable <- rvest::html_table(tables[[1]])

  # > dplyr::glimpse(tzTable)
  # Rows: 591
  # Columns: 8
  # $ `Country code`                            <chr> "CI", "GH", "ET", "DZ", "ER", "M…
  # $ `Latitude, longitude ±DDMM(SS)±DDDMM(SS)` <chr> "+0519−00402", "+0533−00013", "+…
  # $ `TZ database name`                        <chr> "Africa/Abidjan", "Africa/Accra"…
  # $ `Portion of country covered`              <chr> "", "", "", "", "", "", "", "", …
  # $ Status                                    <chr> "Canonical", "Canonical", "Alias…
  # $ `UTC offset ±hh:mm`                       <chr> "+00:00", "+00:00", "+03:00", "+…
  # $ `UTC DST offset ±hh:mm`                   <chr> "+00:00", "+00:00", "+03:00", "+…
  # $ Notes                                     <chr> "", "", "Link to Africa/Nairobi"…
  
  # Rationalize naming:
  # * human readable full nouns with descriptive prefixes
  # * generally lowerCamelCase
  # with internal standards:
  # * timezone (Olson timezone)
  # * longitude (decimal degrees E)
  # * latitude (decimal degrees N)
  names(tzTable) <- c('countryCode','coordinates','timezone','comments','status','UTC_offset','UTC_DST_offset','notes')
  
  # NOTE:  rvest::html_table has no argument to specify na.strings so "NA" is converted to NA
  tzTable$countryCode[tzTable$timezone == 'Africa/Windhoek'] <- "NA"
  
  # Remove all rows where the Notes say "Link to ..."
  tzTable <- tzTable[!stringr::str_detect(tzTable$notes,'^Link to'),]

  # NOTE:  On 2020-07-22, the Entry for "Eurpoe/Sarajevo" incorreclty uses the 
  # NOTE:  countryCode for Bahrain (BH) instead of for Bosnia (BA)
  
  # > tzTable %>% filter(timezone == "Europe/Sarajevo")
  # countryCode coordinates        timezone comments    status UTC_offset UTC_DST_offset notes
  # 1          BH +4450+02030 Europe/Sarajevo          Canonical     +01:00         +02:00      

  tzTable$countryCode[tzTable$timezone == 'Europe/Sarajevo'] <- "BA"
  
  # Convert UTC_offset "+HH:MM" to hours
  sign <- ifelse(stringr::str_sub(tzTable$UTC_offset,1,1) == '+',1,-1)
  hour <- as.numeric(stringr::str_sub(tzTable$UTC_offset,2,3))
  min <- as.numeric(stringr::str_sub(tzTable$UTC_offset,5,6))
  tzTable$UTC_offset <- sign * (hour + min/60)
  
  # Convert UTC_DST_offset "+HH:MM" to hours
  sign <- ifelse(stringr::str_sub(tzTable$UTC_DST_offset,1,1) == '+',1,-1)
  hour <- as.numeric(stringr::str_sub(tzTable$UTC_DST_offset,2,3))
  min <- as.numeric(stringr::str_sub(tzTable$UTC_DST_offset,5,6))
  tzTable$UTC_DST_offset <- sign * (hour + min/60)
  
  # Create longitude and latitude
  matchMatrix <- stringr::str_match(tzTable$coordinates,'([+-][0-9]+)([+-][0-9]+)')
  
  # Latitudes -- 5 or 7 characters
  latString <- matchMatrix[,2]
  sign <- ifelse(stringr::str_sub(latString,1,1) == '+',1,-1)
  deg <- as.numeric(stringr::str_sub(latString,2,3))
  min <- as.numeric(stringr::str_sub(latString,4,5))
  sec <- as.numeric(stringr::str_sub(latString,4,5))
  sec <- ifelse(is.na(sec),0,sec)
  tzTable$latitude <- sign * (deg + min/60 + sec/3600) 
  
  # Longitudes -- 6 or 8 characters
  lonString <- matchMatrix[,3]
  sign <- ifelse(stringr::str_sub(lonString,1,1) == '+',1,-1)
  deg <- as.numeric(stringr::str_sub(lonString,2,4))
  min <- as.numeric(stringr::str_sub(lonString,5,6))
  sec <- as.numeric(stringr::str_sub(lonString,7,8))
  sec <- ifelse(is.na(sec),0,sec)
  tzTable$longitude <- sign * (deg + min/60 + sec/3600) 
  
  # Return desired columns in a sensible order
  return( tzTable[,c('timezone','UTC_offset','UTC_DST_offset','countryCode','longitude','latitude')] )
   
}
