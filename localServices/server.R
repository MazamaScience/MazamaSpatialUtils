library(shiny)
library(MazamaSpatialUtils)
library(ggplot2)
setSpatialDataDir("~/Data/Spatial/")

function(input, output){
  output$myPlot <- renderPlot({
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)
    inputData <- read.csv(inFile$datapath)
    inputData <- subset(inputData, longitude < -80)
    plot(inputData$longitude, inputData$latitude)
  })
}