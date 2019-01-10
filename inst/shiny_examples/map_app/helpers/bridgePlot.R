# Function which plots a map representing bridge data
# based on user parameters

bridgePlot <- function(data, 
                       outputData, 
                       SPDF, 
                       FUN, 
                       style, 
                       title = "", 
                       stateOutline = NULL) {
  
  logger.trace("bridgePlot()")
  logger.trace(str(list(style = style)))
  
  # Plot colors by quantiles
  breaks <- quantile(subset(outputData, summaryValue > 0)$summaryValue)
  colIndices <- .bincode(outputData$summaryValue, breaks)
  legendColors <- RColorBrewer::brewer.pal(4, 'Blues')
  colors <- legendColors[colIndices]
  colors[is.na(colors)] <- 'gray90'
  
  if (style == "base_spdf_plus_points") {
    
    logger.trace("plotting...")
    plot(SPDF, col = colors)
    plot(stateOutline, add=TRUE)
    points(data$longitude, data$latitude, pch = 2)
    legend("topright", 
           legend = names(breaks)[1:4], 
           fill = legendColors, 
           title = "Density by area")
    title(title)
    
  } else if (style == "points_plus_state") {
    
    logger.trace("plotting...")
    plot(stateOutline)
    points(data$longitude, data$latitude, pch = 2)
  
  } else if (style == "base_spdf") {
    
    logger.trace("plotting...")
    plot(SPDF, col = colors)
    plot(stateOutline, add=TRUE)
    legend("topright", 
           legend = names(breaks)[1:4], 
           fill = legendColors, 
           title = "Density by area")
    title(title)
    
  }
}
