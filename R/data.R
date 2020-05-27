#' @export
#' @docType data
#' @name US_stateCodes
#' @title Dataframe of US state codes
#' @format A dataframe with 51 rows and 6 columns of data.
#' @description US_stateCodes the following columns for US states and territories:
#' \itemize{
#' \item{\code{stateName} -- English language state name}
#' \item{\code{stateCode} -- ISO 3166-2 alpha-2}
#' \item{\code{stateFIPS} -- two-digit FIPS code}
#' }
#' @references \href{https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code}{Wikipedia}
 
US_stateCodes <- data.frame(
  stateName = c(
    "Alabama", "Alaska", "American Samoa", 
    "Arizona", "Arkansas", "Baker Island", "California", "Colorado", 
    "Connecticut", "Delaware", "District of Columbia", "Federated States of Micronesia", 
    "Florida", "Georgia", "Guam", "Hawaii", "Howland Island", "Idaho", 
    "Illinois", "Indiana", "Iowa", "Jarvis Island", "Johnston Atoll", 
    "Kansas", "Kentucky", "Kingman Reef", "Louisiana", "Maine", "Marshall Islands", 
    "Maryland", "Massachusetts", "Michigan", "Midway Islands", "Minnesota", 
    "Mississippi", "Missouri", "Montana", "Navassa Island", "Nebraska", 
    "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", 
    "North Carolina", "North Dakota", "Northern Mariana Islands", 
    "Ohio", "Oklahoma", "Oregon", "Palau", "Palmyra Atoll", "Pennsylvania", 
    "Puerto Rico", "Rhode Island", "South Carolina", "South Dakota", 
    "Tennessee", "Texas", "U.S. Minor Outlying Islands", "Utah", 
    "Vermont", "Virgin Islands of the U.S.", "Virginia", "Wake Island", 
    "Washington", "West Virginia", "Wisconsin", "Wyoming"
  ), 
  stateCode = c(
    "AL", 
    "AK", "AS", "AZ", "AR", "BI", "CA", "CO", "CT", "DE", "DC", "FM", 
    "FL", "GA", "GU", "HI", "HI", "ID", "IL", "IN", "IA", "JI", "JA", 
    "KS", "KY", "KR", "LA", "ME", "MH", "MD", "MA", "MI", "MI", "MN", 
    "MS", "MO", "MT", "NI", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", 
    "ND", "MP", "OH", "OK", "OR", "PW", "PA", "PA", "PR", "RI", "SC", 
    "SD", "TN", "TX", "UM", "UT", "VT", "VI", "VA", "WI", "WA", "WV", 
    "WI", "WY"
  ), 
  stateFIPS = c(
    "01", "02", "60", "04", "05", "81", 
    "06", "08", "09", "10", "11", "64", "12", "13", "66", "15", "84", 
    "16", "17", "18", "19", "86", "67", "20", "21", "89", "22", "23", 
    "68", "24", "25", "26", "71", "27", "28", "29", "30", "76", "31", 
    "32", "33", "34", "35", "36", "37", "38", "69", "39", "40", "41", 
    "70", "95", "42", "72", "44", "45", "46", "47", "48", "74", "49", 
    "50", "78", "51", "79", "53", "54", "55", "56"
  ),
  stringsAsFactors = FALSE
)


#' @export
#' @docType data
#' @name CONUS
#' @title CONUS state codes
#' @format A vector with 49 elements
#' @description
#' State codes for the 48 contiguous states +DC that make up the CONtinental US.

CONUS <- c(     "AL","AZ","AR","CA","CO","CT","DE","FL","GA",
                "ID","IL","IN","IA","KS","KY","LA","ME","MD",
                "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
                "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
                "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY",
                "DC"     )

#' @export
#' @docType data
#' @name US_52
#' @title US state codes
#' @format A vector with 52 elements
#' @description
#' State codes for the 50 states +DC +PR (Puerto Rico).

US_52 <- c("AK","AL","AZ","AR","CA","CO","CT","DE","FL","GA",
           "HI","ID","IL","IN","IA","KS","KY","LA","ME","MD",
           "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
           "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
           "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY",
           "DC","PR")
