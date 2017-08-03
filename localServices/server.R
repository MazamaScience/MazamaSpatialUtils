library(shiny)
library(MazamaSpatialUtils)
library(sp)

#this is a hard-coded link for dev purposes
setSpatialDataDir("~/Data/Spatial/")

function(input, output, session){
  output$myPlot <- renderPlot({
    inFile <- input$file1

    req(inFile)
    inputData <- read.csv(inFile$datapath, sep = input$sep, quote = input$quote)
    vars <- names(inputData)
    vars <- vars[vars != c('latitude', 'longitude')]

    loadSpatialData(input$SPDF)
    sessionSPDF <- subset(eval(parse(text = input$SPDF)), stateCode == 'WA')
  
    
    forPlot <- summarizeByPolygon(inputData$longitude, inputData$latitude, 
                                  value = inputData[, 3],
                                  SPDF = sessionSPDF, FUN = eval(parse(text = input$FUN)))
    forPlot <- na.omit(forPlot)

    # Get the correct plot order
    plotOrder <- sessionSPDF$HUC[sessionSPDF$HUC %in% forPlot$polygonID]
    plotOrder <- as.data.frame(plotOrder)
    names(plotOrder) <- "polygonID"
    plotDF <- dplyr::left_join(plotOrder, forPlot, by='polygonID')

    # Plot colors by quantiles
    breaks <- quantile(forPlot$summaryValue)
    colIndexes <- .bincode(plotDF$summaryValue, breaks)
    colors <- RColorBrewer::brewer.pal(4, 'Blues')
    cols <- colors[colIndexes]

    plot(sessionSPDF, col = cols)
    legend("topright", legend = names(breaks), fill = cols, title = "Density by area")
    title(paste(parse(text=input$SPDF), "with", parse(text=input$FUN), "function"))
  })
  output$myTable <- renderTable({
    inFile <- input$file1
    
    req(inFile)
    inputData <- read.csv(inFile$datapath, sep = input$sep, quote = input$quote)
    vars <- names(inputData)
    vars <- vars[vars != c('latitude', 'longitude')]
    updateSelectInput(session, "columns","Select Columns", choices = vars)
    
    loadSpatialData(input$SPDF)
    sessionSPDF <- subset(eval(parse(text = input$SPDF)), stateCode == 'WA')
    
    forPlot <- summarizeByPolygon(inputData$longitude, inputData$latitude, 
                                  value = inputData[, 3],
                                  SPDF = sessionSPDF, FUN = eval(parse(text = input$FUN)))
    forPlot <- na.omit(forPlot)
    
    # Get the correct plot order
    plotOrder <- sessionSPDF$HUC[sessionSPDF$HUC %in% forPlot$polygonID]
    plotOrder <- as.data.frame(plotOrder)
    names(plotOrder) <- "polygonID"
    plotDF <- dplyr::left_join(plotOrder, forPlot, by='polygonID')
  })
}
