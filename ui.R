library(leaflet)
library(shiny)

ui <- fillPage(
    sidebarLayout(
        sidebarPanel(width = 3, style = "position:fixed;width:inherit;overflow-y:scroll;",
            sliderInput(inputId = "grosor",
                        label = "Grosor de la linea de la grilla",
                        min = 0,
                        max = 1,
                        value = 0.03,
                        pre = "Grosor: "),
            sliderInput(inputId = "tamaño",
                        label = "Tamaño (km) de las celdas de la grilla",
                        min = 10,
                        max = 100,
                        value = 25,
                        pre = "Tamaño: ",
                        post = " km"),
            selectInput(inputId = "especie",
                        label = "Especie",
                        choices = c()),
            sliderInput(inputId = "año",
                        label = "Año",
                        value = c(2000, 2020),
                        min = 2000,
                        max = 2020,
                        pre = "Año:"),
            selectInput(inputId = "impacto",
                        label = "Impacto",
                        choices = c()),
            selectInput(inputId = "investigador",
                        label = "Investigador",
                        choices = c())
        ),
        mainPanel(width = 9,
            leafletOutput("mapa")
        ),
        fluid = FALSE
    )
)
