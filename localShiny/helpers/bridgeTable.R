# This function creates a table with bridge data

bridgeTable <- function(data, SPDF, style) {

  # Prepare data for plotting
  df <- summarizeByPolygon(data$longitude, data$latitude,
                           value = data$yearBuilt,
                           SPDF = SPDF, FUN = eval(parse(text = FUN)))
  df[is.na(df)] <- 0
  
  
  # Get the correct plot order
  plotOrder <- SPDF$polygonID[SPDF$polygonID %in% df$polygonID]
  plotOrder <- data.frame(polygonID = plotOrder, stringsAsFactors = FALSE)
  plotDF <- dplyr::left_join(plotOrder, df, by='polygonID')
  
  if (style == "other"){
    data
  } else if (style == "original_plus_summary"){
    tableDF <- merge(data, df)
    tableDF
  } else if (style == "summary_df") {
    tableDF <- dplyr::left_join(plotOrder, df, by='polygonID')
    tableDF
  }
}