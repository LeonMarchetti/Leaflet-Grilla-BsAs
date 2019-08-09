library(leaflet)
library(shiny)

ui <- fillPage(
    sidebarLayout(
        sidebarPanel(width = 3,
            sliderInput(inputId = "grosor",
                        label = "Grosor de la linea de la grilla",
                        min = 0,
                        max = 1,
                        value = 0.5,
                        pre = "Grosor: "),
            sliderInput(inputId = "tamaño",
                        label = "Tamaño (km) de las celdas de la grilla",
                        min = 1,
                        max = 20,
                        value = 10,
                        pre = "Tamaño: ",
                        post = " km"),
            selectInput(inputId = "especie",
                        label = "Especie",
                        choices = c()),
            sliderInput(inputId = "año",
                        label = "Año",
                        value = c(1980, 2001),
                        min = 1980,
                        max = 2001,
                        pre = "Año:")
        ),
        mainPanel(width = 9,
            leafletOutput("mapa")
        ),
        fluid = FALSE
    )
)
