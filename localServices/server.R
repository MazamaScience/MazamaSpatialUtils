library(shiny)
library(MazamaSpatialUtils)
library(sp)

#this is a hard-coded link for dev purposes
setSpatialDataDir("~/Data/Spatial/")

loadSpatialData('NaturalEarthAdm1')
wa_outline <- subset(NaturalEarthAdm1, countryName == "United States" & stateCode == "WA")

function(input, output, session){
  my_data <- reactive({
    inFile <- input$file1
    req(inFile)
    inputData <- reactiveFileReader(10000, session, filePath = inFile$datapath, read.csv,
                                    sep = input$sep, quote = input$quote)
    inputData()
  })

  output$myPlot <- renderPlot({
    loadSpatialData(input$SPDF)
    sessionSPDF <- subset(eval(parse(text = input$SPDF)), stateCode == 'WA')
    forPlot <- summarizeByPolygon(my_data()$longitude, my_data()$latitude,
                                  value = my_data()[, 3],
                                  SPDF = sessionSPDF, FUN = eval(parse(text = input$FUN)))
    forPlot[is.na(forPlot)] <- 0

    # Get the correct plot order
    plotOrder <- sessionSPDF$polygonID[sessionSPDF$polygonID %in% forPlot$polygonID]
    plotOrder <- as.data.frame(plotOrder)
    names(plotOrder) <- "polygonID"
    plotDF <- dplyr::left_join(plotOrder, forPlot, by='polygonID')

    # Plot colors by quantiles
    breaks <- quantile(subset(forPlot, summaryValue > 0)$summaryValue)
    colIndexes <- .bincode(plotDF$summaryValue, breaks)
    colors <- RColorBrewer::brewer.pal(4, 'Blues')
    cols <- colors[colIndexes]

    if(input$output_figure == "base_spdf_plus_points"){
      plot(sessionSPDF, col = cols)
      inFile <- input$file1
      req(inFile)
      inputData <- read.csv(inFile$datapath, sep = input$sep, quote = input$quote)
      points(inputData$longitude, inputData$latitude, pch = 2)
      legend("topright", legend = names(breaks)[1:4], fill = colors, title = "Density by area")
      title(paste(parse(text=input$SPDF), "with", parse(text=input$FUN), "function"))
    }
    else if(input$output_figure == "points_plus_state"){
      inFile <- input$file1
      req(inFile)
      inputData <- read.csv(inFile$datapath, sep = input$sep, quote = input$quote)

      plot(wa_outline)
      points(inputData$longitude, inputData$latitude, pch = 2)
    }

    else {
    plot(sessionSPDF, col = cols)
    legend("topright", legend = names(breaks)[1:4], fill = colors, title = "Density by area")
    title(paste(parse(text=input$SPDF), "with", parse(text=input$FUN), "function"))
    }

  })
  output$myTable <- renderTable({
    loadSpatialData(input$SPDF)
    sessionSPDF <- subset(eval(parse(text = input$SPDF)), stateCode == 'WA')

    forPlot <- summarizeByPolygon(my_data()$longitude, my_data()$latitude,
                                  value = my_data()[, 3],
                                  SPDF = sessionSPDF, FUN = eval(parse(text = input$FUN)))
    forPlot[is.na(forPlot)] <- 0

    plotOrder <- sessionSPDF$polygonID[sessionSPDF$polygonID %in% forPlot$polygonID]
    plotOrder <- as.data.frame(plotOrder)
    names(plotOrder) <- "polygonID"

    if(input$output_file == "other"){
      my_data()
    }
    if(input$output_file == "original_plus_summary"){
      inFile <- input$file1
      req(inFile)
      inputData <- read.csv(inFile$datapath, sep = input$sep, quote = input$quote)
      plotDF <- merge(inputData, forPlot)
      plotDF
    }
    else{
    plotDF <- dplyr::left_join(plotOrder, forPlot, by='polygonID')
    plotDF
    }
  })
  output$downloadData <- downloadHandler(
        filename = 'summary_data.csv',
        content = function(file) {
          write.csv(my_data(), file)
        }
  )

}
