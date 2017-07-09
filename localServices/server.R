library(shiny)
library(MazamaSpatialUtils)
library(sp)

#this is a hard-coded link for dev purposes
setSpatialDataDir("~/Data/Spatial/")

function(input, output){
  output$myPlot <- renderPlot({
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)
    inputData <- read.csv(inFile$datapath)
    inputData <- subset(inputData, longitude < -80)

    loadSpatialData("WBDHU8")
    waHUC8 <- subset(WBDHU8, stateCode == 'WA')

    forPlot <- summarizeByPolygon(inputData$longitude, inputData$latitude, value = inputData$age,
                                  SPDF = waHUC8, polygonName = "HUCName", FUN = mean)
    forPlot <- na.omit(forPlot)

    # Get the correct plot order
    plotOrder <- waHUC8$HUCName[waHUC8$HUCName %in% forPlot$polygonName]
    plotOrder <- as.data.frame(plotOrder)
    names(plotOrder) <- "polygonName"
    plotDF <- dplyr::left_join(plotOrder, forPlot, by='polygonName')

    # Plot colors by quantiles
    breaks <- quantile(forPlot$summaryValue)
    colIndexes <- .bincode(plotDF$summaryValue, breaks)
    colors <- RColorBrewer::brewer.pal(4, 'Blues')
    cols <- colors[colIndexes]

    plot(waHUC8, col = cols)
  })
}
