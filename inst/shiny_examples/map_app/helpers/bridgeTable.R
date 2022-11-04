# This function creates a table with bridge data

bridgeTable <- function(data, outputData, SFDF, FUN, style, variable) {
  logger.trace("bridgeTable()")
  logger.trace(str(list(FUN = FUN, 
                        style = style)))
  
  names(outputData)[which(names(outputData) == "summaryValue")] <- paste0(FUN, "_", variable)
  if (style == "other"){
    tableDF <- data
  } else if (style == "original_plus_summary"){
    tableDF <- merge(data, outputData)
  } else if (style == "summary_df") {
    tableDF <- outputData
  }
  return(tableDF)
}