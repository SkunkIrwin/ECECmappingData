## ECEC Statewide Mapping Nutrition Provisions ######################################

# load package
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(ggmap)
library(shiny)
library(rsconnect)

########################################################################################

# Read in data
data <- read.csv("Data.csv")
View(data)
str(data)

data$Meal.Provision <- factor(data$Meal.Provision)
data$QualityArea2Rating[data$QualityArea2Rating==""] <- "No NQS Rating" 
data$QualityArea2Rating <- factor(data$QualityArea2Rating, 
    levels =  c("No NQS Rating", "Working Towards NQS", "Meeting NQS", "Exceeding NQS"))

########################################################################################

#Shiny & Leaflet App

# Define Colour Palette
pal <- colorFactor(palette = c("firebrick2", "gold2", "dodgerblue2", "chartreuse2"),
                   levels = c("No NQS Rating", "Working Towards NQS", "Meeting NQS", "Exceeding NQS"),
                   data$QualityArea2Rating)

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

# Define server logic
server <- function(input, output) {
  # leaflet map
  output$myMap = renderLeaflet({
    leaflet(data) %>%
      addTiles() %>%
      addMarkers(clusterOptions = markerClusterOptions(),
                 label = paste(data$ServiceName, "(n = ",data$NumberOfApprovedPlaces,"Approved Places);",
                               "SEIFA = ",data$SEIFA)) %>%
      addCircleMarkers(lng = ~Longitude, lat = ~Latitude, stroke = FALSE, fillOpacity = 0.8, radius = 5,
                       color = pal(data$QualityArea2Rating)) %>%
      addLegend(position = "topright", pal = pal, values = data$QualityArea2Rating,
                title = "NQS Area 2 Rating")
  })
  
  # data table 1
  output$table = renderTable({
    bounds = input$mymap_bounds
    thisDF<-subset(data,Latitude>input$myMap_bounds$south &
                     Latitude<input$myMap_bounds$north &
                     Longitude<input$myMap_bounds$east &
                     Longitude>input$myMap_bounds$west)
    m1<-data.frame(table(thisDF$QualityArea2Rating))
    colnames(m1)<-c('NQS Area 2 Rating','# Centres')
    m1})
  
  # data table 2
  output$table2 = renderTable({
    bounds = input$mymap_bounds
    thisDF<-subset(data,Latitude>input$myMap_bounds$south &
                     Latitude<input$myMap_bounds$north &
                     Longitude<input$myMap_bounds$east &
                     Longitude>input$myMap_bounds$west)
    m1<-data.frame(table(thisDF$SEIFA))
    colnames(m1)<-c('SEIFA Score','# Centres')
    m1})
}

# Run the application
shinyApp(ui = ui, server = server)

# Deploy App
deployApp()


