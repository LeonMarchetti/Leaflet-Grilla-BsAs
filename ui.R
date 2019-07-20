library(leaflet)
library(shiny)

ui <- fluidPage(
    sliderInput(inputId = "grosor",
                label = "Grosor de la linea de la grilla",
                min = 0,
                max = 1,
                value = 0.5,
                width = "100%",
                pre = "Grosor: "),
    leafletOutput("mapa", height=550))
