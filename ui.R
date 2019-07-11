library(leaflet)
library(shiny)

ui <- fluidPage(
    leafletOutput("mapa", height=1280)
)
