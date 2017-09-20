library(shiny)
library(MazamaSpatialUtils)
library(sp)

#this is a hard-coded link for dev purposes
setSpatialDataDir("~/Data/Spatial/")

if (dir.exists("images")){
} else {
  dir.create("images")
}

loadSpatialData('NaturalEarthAdm1')
wa_outline <- subset(NaturalEarthAdm1, countryName == "United States" & stateCode == "WA")

function(input, output, session){
  input_data <- reactive({
    inFile <- input$file1
    req(inFile)
    inputData <- reactiveFileReader(10000, session, filePath = inFile$datapath, read.csv,
                                    sep = input$sep, quote = input$quote)
    inputData()
  })

  hash_string <- reactive({fn <- digest::digest(c(input$file1$name, input$SPDF, input$FUN, input$output_figure))
    fn
    })

  output$myPlot <- renderImage({
    filename <- paste0(hash_string(), ".png")
    filepath <- paste0("images/", filename)
    if (filename %in% list.files("images")){
      list(src=filepath)
    } else {

    loadSpatialData(input$SPDF)
    sessionSPDF <- subset(eval(parse(text = input$SPDF)), stateCode == 'WA')
    forPlot <- summarizeByPolygon(input_data()$longitude, input_data()$latitude,
                                  value = input_data()[, 3],
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

      #png(filepath)
      png(filepath, width=780, height=780, units="px")
      plot(sessionSPDF, col = cols)
      points(input_data()$longitude, input_data()$latitude, pch = 2)
      legend("topright", legend = names(breaks)[1:4], fill = colors, title = "Density by area")
      title(paste(parse(text=input$SPDF), "with", parse(text=input$FUN), "function"))
      dev.off()
      list(src=filepath)
    }
    else if  (input$output_figure == "points_plus_state"){

      png(filepath, width=780, height=780, units="px")
      plot(wa_outline)
      points(input_data()$longitude, input_data()$latitude, pch = 2)
      dev.off()
      list(src=filepath)
    }

    else {

    #png(filepath)
    png(filepath, width=780, height=780, units="px")
    plot(sessionSPDF, col = cols)
    legend("topright", legend = names(breaks)[1:4], fill = colors, title = "Density by area")
    title(paste(parse(text=input$SPDF), "with", parse(text=input$FUN), "function"))
    dev.off()
    list(src=filepath)
    }
    }
  }, deleteFile=FALSE)
  output$myTable <- renderTable({
    loadSpatialData(input$SPDF)
    sessionSPDF <- subset(eval(parse(text = input$SPDF)), stateCode == 'WA')

    forPlot <- summarizeByPolygon(input_data()$longitude, input_data()$latitude,
                                  value = input_data()[, 3],
                                  SPDF = sessionSPDF, FUN = eval(parse(text = input$FUN)))
    forPlot[is.na(forPlot)] <- 0

    plotOrder <- sessionSPDF$polygonID[sessionSPDF$polygonID %in% forPlot$polygonID]
    plotOrder <- as.data.frame(plotOrder)
    names(plotOrder) <- "polygonID"

    if (input$output_file == "other"){
      input_data()
    }
    if (input$output_file == "original_plus_summary"){
      plotDF <- merge(input_data(), forPlot)
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
         write.csv(input_data(), file)
        }
  )

}
