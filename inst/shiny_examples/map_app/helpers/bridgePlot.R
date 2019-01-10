# Function which plots a map representing bridge data
# based on user parameters

bridgePlot <- function(data, outputData, SPDF, FUN, style, title = "", stateOutline = NULL) {
  
  logger.trace("bridgePlot()")
  logger.trace(str(list(style = style)))
  
  # Plot colors by quantiles
  breaks <- quantile(subset(outputData, summaryValue > 0)$summaryValue)
  colIndices <- .bincode(outputData$summaryValue, breaks)
  colorPalette <- RColorBrewer::brewer.pal(4, 'Blues')
  colors <- colorPalette[colIndices]
  
  if (style == "base_spdf_plus_points"){
    
    logger.trace("plotting...")
    plot(SPDF, col = colors)
    points(data$longitude, data$latitude, pch = 2)
    legend("topright", legend = names(breaks)[1:4], fill = colors, title = "Density by area")
    title(title)
    
  } else if (style == "points_plus_state"){
    
    logger.trace("plotting...")
    plot(stateOutline)
    points(data$longitude, data$latitude, pch = 2)
  
  } else if (style == "base_spdf") {
    
    logger.trace("plotting...")
    plot(SPDF, col = colors)
    legend("topright", legend = names(breaks)[1:4], fill = colors, title = "Density by area")
    title(title)
    
  }
}
