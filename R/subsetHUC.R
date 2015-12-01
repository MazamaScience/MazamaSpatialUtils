#' @keywords datagen
#' @export
#' @title Subset pre-formatted HUC files into smaller groupings. 
#' @param SPDF a spatial polygons dataframe created using the convertUSGSHUC function
#' @param subsetHUC a character string that specifies which HUC to subset by
#' @param stateCode a character string specifying which state to subset by
#' @param allStateCodes similar to stateCode, but will also include HUCs who touch the 
#' state but whose centroid is in a different state. 
#' @description A SpatialPolygons Dataframe is broken into smaller pieces based on
#' HUC code or state. The SpatialPolygons Dataframe must have the required fields 
#' 'stateCode', 'HUC', and 'allStateCodes' and is intended to come from the convertUSGSHUC function.
#' The difference between stateCode and allStateCodes is that stateCode has just one two-digit ISO 
#' code while allStateCodes can have more than one. This allows us to include in the subset HUCs where part of the 
#' watershed is in the specified state even though the centroid is in a different state. 
#' @return a SpatialPolygons Dataframe subsetted to the appropriate specifications.   
#' 


subsetHUC <- function(SPDF, subsetHUC=NULL, stateCode=NULL, allStateCodes=NULL){
  
  
  #Check that names of SPDF have required fields
  
  requiredFields <- c('stateCode', 'HUC', 'allStateCodes')
  missingFields <- setdiff(requiredFields, names(SPDF))
  if ( length(missingFields) > 0 ) {
    stop(paste0('Missing fields in SPDF: ',missingFields))
  }
  
  
  #subsest HUC by HUC code
  
  regex <- paste0('^', subsetHUC)
  if (!is.null(subsetHUC)){
    subsetHUC <- as.character(subsetHUC)
    if (stringr::str_length(subsetHUC) > stringr::str_length(SPDF@data$HUC[1])){
      print('Innapropriate subsetHUC level, please choose a level smaller than the original HUC level.')
      stop 
    } else{
    HUCMask <- stringr::str_detect(SPDF@data$HUC, regex)
    SPDF <- SPDF[HUCMask,]
    }
  }
 
  
  #Subset HUC by stateCode
  if ( !is.null(stateCode) ){
    stateMask <- SPDF@data$stateCode == stateCode
    stateMask <- stateMask & !is.na(stateMask)
    SPDF <- SPDF[stateMask,]
  }
  
  #SubsetHUC by allstateCodes
  if (!is.null(allStateCodes)){
    allStateMask <- stringr::str_detect(SPDF@data$allStateCodes, allStateCodes)
    SPDF<- SPDF[allStateMask,]
  }
  
  
  
  
  return(SPDF)
  
}

