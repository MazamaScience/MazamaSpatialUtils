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
      selectInput('output_figure', 'Desired output figure style',
                  c(`Colored SPDF` = 'base_spdf',
                    `Colored SPDF with points` = 'base_spdf_plus_points',
                    `Points with state outline` = 'points_plus_state')),
      selectInput('output_file', 'Desired output file contents',
                  c(`Summary values only` = 'summary_df',
                    `Original data plus summary values` = 'original_plus_summary')),
      downloadButton('downloadData', 'Download')
    ),
    mainPanel(
      plotOutput('myPlot'),
      tableOutput('myTable')
    )
  )
)
