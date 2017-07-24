#' @keywords datagen
#' @export
#' @title Convert ISO country code Table to Dataframe
#' @description Returns a dataframe version of the country code table with the following columns:
#' \itemize{
#'   \item{countryName}
#'   \item{countryCode}
#'   \item{ISO3}
#'   \item{ISONumeric}
#' }
#' @return Dataframe with 247 rows and 4 columns.
#' @references \url{http://www.nationsonline.org/oneworld/country_code_list.htm}
convertISOCodeTable <- function() {
  
  url <- "http://www.nationsonline.org/oneworld/country_code_list.htm"
  
  # Get the raw html from the url
  ISODoc <- xml2::read_html(url)
  
  # Get a list of tables in the document
  tables <- rvest::html_nodes(ISODoc, "table")
  
  # Assume the relevant list is the first table and parse that into a dataframe
  ISOTable <- rvest::html_table(tables[[3]])
  
  # Rationalize naming:
  # * human readable full nouns with descriptive prefixes
  # * generally lowerCamelCase
  
  # > names(ISOTable)
  # [1] ""                                                  "Country or Area Name"                             
  # [3] "ISO \"ALPHA-2 Code"                                "ISO ALPHA-3 Code"                                 
  # [5] "ISO Numeric Code\n          UN M49 Numerical Code"
  
  # Remove first column (was flag images)
  # and empty first row
  ISOTable <- ISOTable[,-1]
  ISOTable <- ISOTable[-1,]
  
  names(ISOTable) <- c('countryName',"countryCode", "ISO3", "ISONumericCode")
  
  # The country code for Namibia is "NA", which was converted to <NA>
  
  ISOTable$countryCode[ISOTable$countryName == "Namibia"] <- "NA"
  
  return(ISOTable)
}
