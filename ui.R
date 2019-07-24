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
    fluidRow(
        column(6,
            selectInput(inputId = "especie",
                        label = "Especie",
                        choices = c())),
        column(6,
            selectInput(inputId = "año",
                        label = "Año",
                        choices = c()))
    ),
    leafletOutput("mapa", height=450))
