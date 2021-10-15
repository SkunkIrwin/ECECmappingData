library(leaflet)

# Define UI
ui <- fluidPage(
  # leaflet box
  #column(
  leafletOutput("myMap", height = 1000
                #width = 8,
                #height= 10
  ),
  #data table box
  column(
    #h4("NQS Ratings"),
    tableOutput("table"),
    width = 4
  ),
  #data table box
  column(
    #h4("SEIFA Ranking"),
    tableOutput("table2"),
    width = 4
  )
)