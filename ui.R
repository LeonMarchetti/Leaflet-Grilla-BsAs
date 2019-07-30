library(leaflet)
library(shiny)

ui <- fluidPage(
    fluidRow(
        column(6, sliderInput(inputId = "grosor",
                              label = "Grosor de la linea de la grilla",
                              min = 0,
                              max = 1,
                              value = 0.5,
                              width = "100%",
                              pre = "Grosor: ")),
        column(6, sliderInput(inputId = "tamaño",
                              label = "Tamaño (km) de las celdas de la grilla",
                              min = 10,
                              max = 100,
                              value = 25,
                              width = "100%",
                              pre = "Tamaño: "))
    ),
    fluidRow(
        column(6,
            selectInput(inputId = "especie",
                        label = "Especie",
                        choices = c(),
                        width = "100%")),
        column(6,
            selectInput(inputId = "año",
                        label = "Año",
                        choices = c(),
                        width = "100%"))
    ),
    leafletOutput("mapa", height=450))
