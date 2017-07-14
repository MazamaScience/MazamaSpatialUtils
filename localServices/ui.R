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
      checkboxInput('header', 'Header', TRUE),
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
      radioButtons('SPDF', 'Spatial Polygons DataFrame',
                   c(HUC2 = 'WBDHU2',
                     HUC4 = 'WBDHU4',
                     HUC6 = 'WBDHU6',
                     HUC8 = 'WBDHU8',
                     HUC10 = 'WBDHU10'),
                   selected = 'WBDHU6'),
      radioButtons('FUN', 'Function',
                   c(MIN = 'min',
                     MEAN = 'mean',
                     MEDIAN = 'median',
                     MAX = 'max'))
    ),
    mainPanel(
      plotOutput('myPlot')
    )
  )
)
