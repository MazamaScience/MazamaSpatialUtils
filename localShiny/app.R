library(shiny)
library(MazamaSpatialUtils)
library(sp)

# shiny options
shinyOptions(cache = diskCache("images"))

# Prep
if (!dir.exists("images")) dir.create("images")
setSpatialDataDir("~/Data/Spatial/")
loadSpatialData('NaturalEarthAdm1')
wa_outline <- subset(NaturalEarthAdm1, countryName == "United States" & stateCode == "WA")

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
      
      # File input settings
      fileInput('fileName', 'Choose CSV File',
                accept=c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv')),
      tags$hr(),
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
      
      # Plot settings
      selectInput('SPDF', 'Spatial Polygons DataFrame',
                  c(GADM = 'GADM_GE_0',
                    `Natural Earth` ='NaturalEarthAdm1',
                    `Time Zone` = 'TMWorldBorders',
                    `Time Zone Simple` = 'TMWorldBordersSimple',
                    `US Census` = 'USCensusCounties',
                    HUC2 = 'WBDHU2',
                    HUC4 = 'WBDHU4',
                    HUC6 = 'WBDHU6',
                    HUC8 = 'WBDHU8',
                    HUC10 = 'WBDHU10',
                    HUC12 = 'WBDHU12'),
                  selected = 'WBDHU6'),
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
      plotOutput('bridgeMap', width="50%", height = "50%"),
      tableOutput('bridgeTable')
    )
  )
)


# Server ------------------------------------------------------------------



server <- function(input, output, session){
  
  # Define reactive variables
  reactiveInputData <- reactive({
    inputFile <- input$fileName
    req(inputFile)
    data <- reactiveFileReader(10000, session, filePath = inputFile$datapath, read.csv,
                                    sep = input$sep, quote = input$quote)
    return(data())
  })
  
  
  reactiveSPDF <- reactive({
    if (!exists(input$SPDF)) {
      loadSpatialData(input$SPDF)
    }
    SPDF <- subset(eval(parse(text = input$SPDF)), stateCode == 'WA')
    return(SPDF)
  })
  
  
  # Define output
  # https://shiny.rstudio.com/articles/plot-caching.html
  output$bridgeMap <- renderCachedPlot(
    expr = {
      title <- paste(parse(text=input$SPDF), "with", parse(text=input$FUN), "function")
      bridgePlot(reactiveInputData(), reactiveSPDF(), input$FUN, input$style, title)
    },
    cacheKeyExpr = {
      list(input$fileName, input$SPDF, input$FUN, input$style)
    }
  )
  
  output$bridgeTable <- renderTable({
    
    bridgeTable(reactiveInputData(), reactiveSPDF(), input$output_file)
    
  })
  
  output$downloadData <- downloadHandler(
    filename = 'summary_data.csv',
    content = function(file) {
      write.csv(reactiveInputData(), file)
    }
  )
  
}

shinyApp(ui, server)
