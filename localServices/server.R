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
    inputData <- read.csv(inFile$datapath, sep = input$sep, quote = input$quote)
    inputData <- subset(inputData, longitude < -80)

    loadSpatialData(input$SPDF)
    sessionSPDF <- subset(eval(parse(text = input$SPDF)), stateCode == 'WA')

    forPlot <- summarizeByPolygon(inputData$longitude, inputData$latitude, value = inputData$age,
                                  SPDF = sessionSPDF, polygonName = "HUCName", FUN = eval(parse(text = input$FUN)))
    forPlot <- na.omit(forPlot)

    # Get the correct plot order
    plotOrder <- sessionSPDF$HUCName[sessionSPDF$HUCName %in% forPlot$polygonName]
    plotOrder <- as.data.frame(plotOrder)
    names(plotOrder) <- "polygonName"
    plotDF <- dplyr::left_join(plotOrder, forPlot, by='polygonName')

    # Plot colors by quantiles
    breaks <- quantile(forPlot$summaryValue)
    colIndexes <- .bincode(plotDF$summaryValue, breaks)
    colors <- RColorBrewer::brewer.pal(4, 'Blues')
    cols <- colors[colIndexes]

    plot(sessionSPDF, col = cols)
  })
}
