library(shiny)

fluidPage(
  titlePanel("Uploading File for use in SummarizePolygons"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
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
      sliderInput("colors", label = "Number of colors", min = 2, 
                  max = 10, value = 4),
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
                   selected = 'mean')
    ),
    mainPanel(
      plotOutput('myPlot'),
      tableOutput('myTable')
    )
  )
)
