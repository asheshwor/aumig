library(shiny)
library(ggplot2)
# UI details for mapping international migration app
shinyUI(fluidPage(theme = "bootstrap.css",
  # App title
  title="Australia inter-state migration data visualization",
  h3("Visualising inter-state migration in Australia"),
  ## row for input and basic numbers output
  fluidRow(
    column(2,
    selectInput("arcType", "Select arc type",
                c('Great circle arc' = 'gc',
                  'Straight line' = 'stl',
                  'Sine curve' = 'sine',
                  selected=NULL)),
selectInput("Origin colour", "Select arc origin colour",
            c("Dark", "Light",
              selected=NULL))
),
column(1,
    radioButtons("radio", "Select year",
                choices = list("2013" = 2013,
                               "2010" = 2010,
                               "2000" = 2000,
                               "1990" = 1990
                               ))
  ),
column(3,
       sliderInput('summaryRows', 'Select number of rows for summary output',
                   min=1, max=20,
                   value=5, step=1)
),
column(2,
       verbatimTextOutput("oid1")
),
column(2,
       actionButton("drawMap","drawmap",styleclass="inverse2",icon = "ok")
),
column(2,
       verbatimTextOutput("oid3")
)

),

  ##Main panel to display some migration statistics on the selected region and
  ##  map of in and out migrants from the region

#          h3("Stock migrant map"),
         plotOutput("mapPlot"),

fluidRow(
  column(6,
h4("Top destination regions"),
tableOutput("outSummary")),
  column(6,
         h4("Top source regions"),
         tableOutput("inSummary"))),
          h4("Documentation"),
p("This app is aimed at computing the number of migrant stocks of the selected region and visualizing the data by connecting migrant origin and destination with great circle arcs. Select the region or country from the drop down box to update the map and data output. The map theme input changes the colour styles for the output map. The data for four years are available. A summary of top destinations of migrants from the selected region and top source for migrants in the region are displayed as tables. Select the number of top regions to display using the slider. The origin and destination of the arcs are coloured differently to visualize the direction of movement. This app uses quite a large amount of memory when visualizing large number of arcs so please be patient and allow ample time for the map to get updated."),
p("The region / country names are entirely based on the United Nations data (see data sources for detail)."),
         p("Number of lines between two regions based on log of migrants between the two regions. Migrant origin shown in green and migrant destination shown in red. Locations within region selected among the top 15 most populated cities in the region with probability based on the population i.e. cities with higher population are more likely to get selected. The origin and destination of arcs are computed in each run so the maps will vary each time."),
h4("Data sources"),         
p("Data sources: Migration data from United Nations, Department of Economic and Social Affairs, Population Division (2013). Trends in International Migrant Stock: Migrants by Destination and Origin (United Nations database, POP/DB/MIG/Stock/Rev.2013) downloaded from http://esa.un.org/unmigration/TIMSA2013/data/UN_MigrantStock_2013.xls. World map shapefile from NaturalEarthData.com. Location and population of cities from geonames.org.")
  
    
  )
)