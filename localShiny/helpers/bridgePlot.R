# Function which plots a map representing bridge data
# based on user parameters

bridgePlot <- function(data, SPDF, FUN, style, title = "") {
  
  df <- summarizeByPolygon(data$longitude, data$latitude,
                           value = data$yearBuilt,
                           SPDF = SPDF, FUN = eval(parse(text = FUN)))
  df[is.na(df)] <- 0
  
  # Get the correct plot order
  plotOrder <- SPDF$polygonID[SPDF$polygonID %in% df$polygonID]
  plotOrder <- data.frame(polygonID = plotOrder, stringsAsFactors = FALSE)
  plotDF <- dplyr::left_join(plotOrder, df, by='polygonID')
  
  # Plot colors by quantiles
  breaks <- quantile(subset(df, summaryValue > 0)$summaryValue)
  colIndices <- .bincode(plotDF$summaryValue, breaks)
  colorPalette <- RColorBrewer::brewer.pal(4, 'Blues')
  colors <- colorPalette[colIndices]
  
  if (style == "base_spdf_plus_points"){
    
    plot(SPDF, col = colors)
    points(data$longitude, data$latitude, pch = 2)
    legend("topright", legend = names(breaks)[1:4], fill = colors, title = "Density by area")
    title(title)
    
  } else if (style == "points_plus_state"){
    
    plot(wa_outline)
    points(data$longitude, data$latitude, pch = 2)
  
  } else if (style == "base_spdf") {
    
    plot(SPDF, col = colors)
    legend("topright", legend = names(breaks)[1:4], fill = colors, title = "Density by area")
    title(title)
    
  }
}
