#' @keywords datagen
#' @export
#'
#' @title Subset pre-formatted HUC files into smaller groupings.
#'
#' @param SFDF a simple features dataframe created using the convertUSGSHUC
#' function
#' @param parentHUCs Character vector specifying one or more containing HUCs.
#' @param stateCodes Character vector specifying one or more containing states.
#' @param allStateCodes Similar to stateCode, but will also include HUCs who
#' touch the state but whose centroid is in a different state.
#'
#' @description A SpatialPolygons Dataframe is broken into smaller pieces based
#' on HUC code or state. The SpatialPolygons Dataframe must have the required
#' fields 'stateCode', 'HUC', and 'allStateCodes' and is intended to come from
#' the \code{convertUSGSHUC()} function. The difference between stateCode and
#' allStateCodes is that stateCode has just one two-digit ISO code while
#' allStateCodes can have more than one. This allows the subset to include
#' HUCs where part of the watershed is in the specified state even though the
#' centroid is in a different state.
#'
#' @return a SpatialPolygons Dataframe subsetted to the appropriate specifications.
#'

subsetHUC <- function(
  SFDF = NULL,
  parentHUCs = NULL,
  stateCodes = NULL,
  allStateCodes = NULL
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(SFDF)

  # Check that names of SFDF have required fields
  requiredFields <- c('stateCode', 'HUC', 'allStateCodes')
  missingFields <- setdiff(requiredFields, names(SFDF))
  if ( length(missingFields) > 0 ) {
    stop(paste0('Missing fields in SFDF: ', missingFields))
  }

  # ----- Subset ---------------------------------------------------------------

  # Identify HUC string partial matches to use as a mask
  if ( !is.null(parentHUCs) ) {
    HUCMask <- rep(FALSE, nrow(SFDF))
    for (HUC in parentHUCs){
      regex <- paste0('^', HUC)
      mask <- stringr::str_detect(SFDF$HUC, regex)
      HUCMask <- HUCMask | mask
    }
    SFDF <- SFDF[HUCMask,]
  }

  # Subset HUC by stateCode
  if ( !is.null(stateCodes) ) {
    stateMask <- rep(FALSE, nrow(SFDF))
    for (stateCode in stateCodes) {
      stateMask <- stateMask | (SFDF$stateCode == stateCode)
    }
    stateMask <- stateMask & !is.na(stateMask)
    SFDF <- SFDF[stateMask,]
  }

  # SubsetHUC by allstateCodes
  if ( !is.null(allStateCodes) ) {
    allStateCodesMask <- rep(FALSE, nrow(SFDF))
    for (stateCode in allStateCodes) {
      allStateCodesMask <- allStateCodesMask | stringr::str_detect(SFDF$allStateCodes, stateCode)
      # Handle NAs in allStateCodes (e.g. Northerm Mariana Islands in HUC4)
      allStateCodesMask[is.na(allStateCodesMask)] <- FALSE
    }
    SFDF <- SFDF[allStateCodesMask,]
  }

  # ----- Return ---------------------------------------------------------------

  return(SFDF)

}

