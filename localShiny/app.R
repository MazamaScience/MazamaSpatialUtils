library(shiny)
library(MazamaSpatialUtils)
library(MazamaCoreUtils)
library(sp)

# shiny options
shinyOptions(cache = diskCache("localCache"))

# Prep
if (!dir.exists("localCache")) dir.create("localCache")
setSpatialDataDir("~/Data/Spatial/")
loadSpatialData('NaturalEarthAdm1')
wa_outline <- subset(NaturalEarthAdm1, countryName == "United States" & stateCode == "WA")
logger.setup()
logger.setLevel("TRACE")

# Source files
helperFiles <- list.files("helpers")
for (file in helperFiles) {
  source(file.path("helpers", file))
}


# UI ----------------------------------------------------------------------


ui <- fluidPage(
  titlePanel("Uploading File for use in SummarizePolygons"),
  sidebarLayout(
    sidebarPanel(
      
      h2("Input file"),
      
      # File input settings
      fileInput('inputDataFile', 'Choose CSV File',
                accept=c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv')),
      checkboxGroupInput('header', 'CSV file has Header',
                         choices = 'TRUE', selected = 'TRUE'),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
      
      tags$hr(),
      
      h2("Plot Settings"),
      
      # Plot settings
      selectInput('SPDF', 'Spatial Polygons DataFrame',
                  c(HUC4 = 'WBDHU4',
                    HUC6 = 'WBDHU6',
                    HUC8 = 'WBDHU8',
                    HUC10 = 'WBDHU10',
                    HUC12 = 'WBDHU12'),
                  selected = 'WBDHU6'),
      selectInput('variable', 'Variable to Aggregate', 
                  character(0)),
      selectInput('FUN', 'Function',
                  c(MIN = 'min',
                    MEAN = 'mean',
                    MAX = 'max'),
                  selected = 'mean'),
      selectInput('style', 'Desired output figure style',
                  c(`Colored SPDF` = 'base_spdf',
                    `Colored SPDF with points` = 'base_spdf_plus_points',
                    `Points with state outline` = 'points_plus_state')),
      
      # Download data settings
      selectInput('output_file', 'Desired output file contents',
                  c(`Summary values only` = 'summary_df',
                    `Original data plus summary values` = 'original_plus_summary')),
      downloadButton('downloadData', 'Download')
      
    ),
    
    # output plot
    mainPanel(
      plotOutput('bridgeMap', width="80%", height = "80%"),
      tableOutput('bridgeTable')
    )
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session){
  
  # Define reactive variables
  reactiveInputData <- reactive({
    
    logger.trace("reactiveInputData()")
    inputFile <- input$inputDataFile
    req(inputFile)
    data <- reactiveFileReader(10000, session, filePath = inputFile$datapath, readr::read_delim,
                                    delim = input$sep, quote = input$quote)
    
    
    
    return(data())
  })
  
  # Update options for aggregating variable
  observe({
    
    data <- reactiveInputData()
    classes <- sapply(data, class)
    choices <- names(data)[which(classes != "character")]
    
    if (is.null(choices)) {
      choices <- character(0)
    }
    
    # add options for aggregating variable
    logger.trace("updateSelectInput()")
    updateSelectInput(session, "variable",
                      label = 'Variable to Aggregate',
                      choices = choices
    )
  })
  
  
  reactiveSPDF <- reactive({
    
    logger.trace("reactiveSPDF")
    
    if (!exists(input$SPDF)) {
      logger.trace("loading %s", input$SPDF)
      loadSpatialData(input$SPDF)
    }
    SPDF <- subset(eval(parse(text = input$SPDF)), stateCode == 'WA')
    return(SPDF)
  })
  
  reactiveOutputData <- reactive({
    # Do data manipulation once and cache it
    
    logger.trace("reactiveOutputData()")
    uniqueCode <- digest::digest(c("outputData", input$inputDatafile$name, input$SPDF, input$FUN, input$variable))
    filePath <- paste0("localCache/", uniqueCode, ".RData")
    
    if (!file.exists(filePath)) {
      # Get data
      data <- reactiveInputData()
      SPDF <- reactiveSPDF()
      if (!"polygonID" %in% names(SPDF)) {
        SPDF$polygonID <- SPDF$HUC
      }
      
      # Aggregate data based on FUN
      logger.trace("Aggregating data by %s", input$variable)
      df <- summarizeByPolygon(data$longitude, data$latitude,
                               value = data[[input$variable]],
                               SPDF = SPDF, FUN = eval(parse(text = input$FUN)))
      logger.trace("Successfully aggregated data")
      df[is.na(df)] <- 0
      
      # Get the correct plot order
      plotOrder <- SPDF$HUC[SPDF$polygonID %in% df$polygonID]
      plotOrder <- data.frame(polygonID = plotOrder, stringsAsFactors = FALSE)
      df <- dplyr::left_join(plotOrder, df, by='polygonID')
      
      logger.trace("saving %s", filePath)
      save(df, file = filePath)
    } else {
      logger.trace("loading %s", filePath)
      df <- get(load(filePath))
    }
    
    return(df)
    
  })
  
  
  # Define output
  # https://shiny.rstudio.com/articles/plot-caching.html
  output$bridgeMap <- renderCachedPlot(
    expr = {
      title <- paste(parse(text=input$SPDF), "with", parse(text=input$FUN), "function")
      bridgePlot(reactiveInputData(), 
                 reactiveOutputData(), 
                 reactiveSPDF(), 
                 input$FUN, 
                 input$style, 
                 title,
                 wa_outline)
    },
    cacheKeyExpr = {
      list(input$inputDataFile$name, input$SPDF, input$FUN, input$style, input$variable)
    },
    sizePolicy = function(dims){return(c(750,750))}
  )
  
  output$bridgeTable <- renderTable({
    
    bridgeTable(reactiveInputData(), 
                reactiveOutputData(), 
                reactiveSPDF(), 
                input$FUN, 
                input$output_file,
                input$variable)
    
  })
  
  output$downloadData <- downloadHandler(

    filename = 'summary_data.csv',
    content = function(file) {
      logger.trace("download data")
      write.csv(reactiveInputData(), file)
    }
  )
  
}


# Start the app -----------------------------------------------------------

shinyApp(ui, server)
