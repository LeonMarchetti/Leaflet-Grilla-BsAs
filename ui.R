library(leaflet)
library(shiny)

ui <- fillPage(
    sidebarLayout(
        sidebarPanel(width = 3, style = "position:fixed;width:inherit;overflow-y:scroll;height:100vh",
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
            # Especies
            selectizeInput(inputId = "especie",
                        label = "Especie",
                        choices = c(),
                        multiple = TRUE,
                        options = list(placeholder = "Ninguna")),
            actionLink("especie_todos", "Todas"),
            br(),
            actionLink("especie_ninguno", "Ninguna"),

            # Fecha
            sliderInput(inputId = "año",
                        label = "Año",
                        value = c(2000, 2020),
                        min = 2000,
                        max = 2020,
                        pre = "Año:"),

            # Impactos
            selectizeInput(inputId = "impacto",
                        label = "Impacto",
                        choices = c(),
                        multiple = TRUE,
                        options = list(placeholder = "Ninguno")),
            actionLink("impacto_todos", "Todos"),
            br(),
            actionLink("impacto_ninguno", "Ninguno"),

            # Investigadores
            selectizeInput(inputId = "investigador",
                        label = "Investigador",
                        choices = c(),
                        multiple = TRUE,
                        options = list(placeholder = "Ninguno")),
            actionLink("investigador_todos", "Todos"),
            br(),
            actionLink("investigador_ninguno", "Ninguno")
        ),
        mainPanel(width = 9,
            leafletOutput("mapa", height = "100vh")
        ),
        fluid = FALSE
    )
)
