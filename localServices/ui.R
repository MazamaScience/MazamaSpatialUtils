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
                   c(HUC6 = 'WBDHU6',
                     HUC8 = 'WBDHU8',
                     HUC10 = 'WBDHU10'))
    ),
    mainPanel(
      plotOutput('myPlot')
    )
  )
)
