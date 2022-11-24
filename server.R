# Importo la función de extrapolación, que de acuerdo al valor de una celda de
# la grilla calcula un valor para sumar sobre las celdas vecinas.
source("extrapol.R")


library(dplyr)
library(geosphere)
library(htmltools)
library(leaflet)
library(raster)
library(rgdal)
library(rgeos)
# library(RMySQL)
library(sp)
library(stringr)


mostrar <- function(titulo, x) {
    # Muestra una variable por consola
    #
    # Args:
    #   titulo: El título de la exposición de la variable.
    #   x: La variable a mostrar.

    cat("\n", titulo, "\n")
    print(x)
    cat("\n")
}

importar_datos <- function() {
    # Importo los datos de un archivo csv. Los datos tienen el nombre de la
    # especie, el año, la densidad, y su ubicación geográfica.
    #
    # Returns:
    #   Un objeto SpatialPointsDataFrame con las muestras importadas.

    # MySQL:
    # creds <- read.csv("./credenciales.csv", stringsAsFactors = FALSE)
    # if (dbCanConnect(MySQL(), user = creds$user, password = creds$password, dbname = creds$dbname, host = creds$host, port = creds$port)) {
    #     db <- dbConnect(MySQL(), user = creds$user, password = creds$password, dbname = creds$dbname, host = creds$host, port = creds$port)
    #     rs <- dbSendQuery(db, "Call get_muestras()")
    #     lomb.data <- dbFetch(rs)
    #     # Convierto los nombres de especies a Factor
    #     lomb.data$species <- as.factor(lomb.data$species)
    #     dbClearResult(rs)
    #     dbDisconnect(db)
    #
    # } else {
    #     # Si no hay conexión con la bd entonces cargo un dataset de prueba en
    #     # un archivo CSV
    #     lomb.data <- read.csv("./lombriz-data-rand.csv")
    # }

    lomb.data <- read.csv("./Coviella.csv")

    # Convierto los datos importados en puntos espaciales.
    lomb.sp <- lomb.data
    coordinates(lomb.sp) <- ~x+y

    return(lomb.sp)
}

importar_figura <- function() {
    # Importo del archivo la figura sobre donde hacer la grilla.
    #
    # Returns:
    #   Un objeto SpatialPolygons que representa La figura importada.

    # ARG_adm1.shp tiene las formas de las provincias.
    argentina <- readOGR(dsn = "./ARG_adm/ARG_adm1.shp", verbose = FALSE)

    # ARG_adm1.csv tiene los nombres de las provincias para la detección de la
    # forma.
    bsas <- subset(argentina, str_detect(NAME_1, "Buenos Aires"))

    # Simplifico la figura de la provincia para reducir el tiempo de ejecución.
    gSimplify(bsas, tol = 0.05)
}

adaptar_datos_espaciales <- function(lomb.sp, fig) {
    # Adapto el data frame espacial con los datos a una figura.
    #
    # Args:
    #   lomb.sp: El data frame espacial (SpatialPointsDataFrame).
    #   fig: La figura (SpatialPolygons).
    #
    # Returns:
    #   El data frame espacial modificado.

    # Enfuerzo los límites de la figura sobre los puntos
    proj4string(lomb.sp) <- proj4string(fig)

    # Borro las muestras que quedan afuera de la figura.
    lomb.sp[fig, ]
}

actualizar_controles <- function(session, lomb.sp) {
    # Actualizo los controles, cambiando los valores límites o las elecciones
    # posibles, usando los datos importados. Los controles son del framework de
    # Shiny.
    #
    # Args:
    #   session: Objeto sesión de Shiny.
    #   lomb.sp: Un objeto SpatialPointsDataFrame que representa las muestras,
    #            con valor de densidad y ubicación geográfica.

    # Defino los valores posibles para el select de la especie:
    # updateSelectInput(session, inputId = "especie", choices = sort(unique(lomb.sp$species)))
    # updateSelectInput(session, inputId = "especie",
    #                   choices = c("Todos", as.vector(sort(unique(lomb.sp$species)))))
    updateSelectInput(session, inputId = "especie",
                      choices = c("Todos", "No juveniles", as.vector(sort(unique(lomb.sp$species)))))

    # Defino el rango para el deslizador del año, los años mínimo y máximo en
    # la muestra pasan a ser los límites del deslizador:
    min_año = min(lomb.sp$year)
    max_año = max(lomb.sp$year)

    updateSliderInput(session,
                      inputId = "año",
                      min = min_año,
                      max = max_año,
                      value = c(min_año, max_año))

    # updateSelectInput(session, inputId = "impacto", choices = sort(unique(lomb.sp$impact)))
    updateSelectInput(session, inputId = "impacto",
                      choices = c("Todos", as.vector(sort(unique(lomb.sp$impact)))))

    # updateSelectInput(session, inputId = "investigador", choices = sort(unique(lomb.sp$researcher)))
    updateSelectInput(session, inputId = "investigador",
                      choices = c("Todos", as.vector(sort(unique(lomb.sp$researcher)))))
}

dimensiones_celdas <- function(distancia, matriz) {
    # Función que determina la cantidad de filas y columnas debe tener la
    # grilla de la figura en base al tamaño de esta y el que tendría la celda.
    #
    # Args:
    #   distancia: Tamaño en km que debería tener la celda.
    #   matriz: Matriz con las dimensiones de la figura que va a tener la
    #           grilla.
    #
    # Returns:
    #   Un par (filas, columnas) con la cantidad de filas y columnas de la
    #   grilla.

    # Resultado de bbox(fig):
    min_x <- matriz[[1]]
    min_y <- matriz[[2]]
    max_x <- matriz[[3]]
    max_y <- matriz[[4]]

    # Puntos de las esquinas:
    esq_ii <- c(min_x, min_y)
    #esq_id <- c(max_x, min_y)
    esq_si <- c(min_x, max_y)
    esq_sd <- c(max_x, max_y)

    # Distancia de cada punto en kilómetros
    dist_horizontal <- distHaversine(esq_si, esq_sd) / 1000
    dist_vertical <- distHaversine(esq_ii, esq_si) / 1000

    # División de la distancia con el tamaño deseado de la celda:
    filas <- dist_vertical %/% distancia
    columnas <- dist_horizontal %/% distancia

    return(c(filas, columnas))
}

armar_grilla <- function(fig, tam) {
    # Armo la grilla, que se trata de un conjunto de polígonos cuadrados,
    # siguiendo el contorno de la figura.
    #
    # Args:
    #   fig: Figura (SpatialPolygons) donde armar la grilla.
    #   tam: Tamaño en kilómetros de las celdas de la grilla
    #
    # Returns:
    #   Un objeto SpatialPolygons que representa la grilla sobre la figura.

    # Determino el límite rectangular de la figura
    # bbox(fig)
    #         min       max
    # x -63.39386 -56.66736
    # y -41.03542 -33.26014
    e <- extent(bbox(fig))

    # Convierto a objeto raster
    r <- raster(e)

    # Divido en grilla de filas x columnas
    dim(r) <- dimensiones_celdas(tam, bbox(fig))
    projection(r) <- crs(proj4string(fig))

    # Agrego el ID de etiqueta a las celdas
    # * Sacando esta linea se saca la grilla
    r <- setValues(r, 1:ncell(r))

    # Reconvierto en un archivo de forma para crear un popup del ID de celda
    # para cada polígono
    shape <- rasterToPolygons(r, dissolve = TRUE)
    crs(shape) <- crs(fig)

    # Recorto las celdas de la grilla que contengan el polígono de la figura.
    p <- shape[fig, ]

    # Recorto el perímetro de la grilla para coincidir con el polígono de la
    # figura.
    gIntersection(p, fig, byid = TRUE, drop_lower_td = TRUE)
}

agrupar_muestras <- function(lomb.sp, grilla) {
    # Agrupa cada muestra en la grilla de la figura, y calcula el promedio en
    # cada una de las celdas. Luego calcula un valor de extrapolación para cada
    # celda y lo suma a cada celda vecina.
    #
    # Args:
    #   lomb.sp: Un objeto SpatialPointsDataFrame que representa las muestras,
    #            con valor de densidad y ubicación geográfica.
    #   grilla: Un objeto SpatialPolygons que representa la grilla.
    #
    # Returns:
    #   Un objeto SpatialPolygonsDataFrame que representa a las muestras
    #   agrupadas en cada celda de la grilla y con un valor de densidad
    #   promedio calculado para cada celda.
    #
    agg <- aggregate(lomb.sp, grilla, function(x) suppressWarnings(mean(as.numeric(x))))

    # Lista de vecinos de todos los polígonos
    list.vec <- gTouches(grilla, byid = TRUE, returnDense = FALSE)

    # Calculo los valores a sumar para cada celda. Como la agregación me
    # devuelve una lista para representar la grilla, entonces voy a calcular
    # la posición de la siguiente celda según su índice y la cantidad de filas
    # y columnas.
    list_extrapol <- sapply(1:length(agg), function(i) {
        x <- 0

        # Itero sobre todas las celdas vecinas de la celda "i":
        for (vecino in list.vec[[i]]) {

            # Sumo el valor extrapolado de la densidad de la celda vecina:
            if (!is.na(agg$dens[[vecino]])) {
                x <- x + extrapol(agg$dens[[vecino]])
            }
        }
        return(x)
    })

    # Sumo los valores extrapolados a la agregación:
    for (i in 1:length(agg)) {
        if (list_extrapol[[i]] != 0 & is.na(agg$dens[[i]]) ) {
            agg$dens[[i]] <- 0
        }
        agg$dens[[i]] <- agg$dens[[i]] + list_extrapol[[i]]
    }

    return(agg)
}

armar_paleta <- function(agg) {
    # Creo la paleta de colores, según los valores de la densidad de las celdas
    # de la grilla.
    #
    # Args:
    #   agg: Un objeto SpatialPolygonsDataFrame, que representa la grilla con
    #        las muestras agrupadas en cada celda.
    #
    # Returns:
    #   Una función que toma un parámetro que calcula un color según ese valor.

    qpal <- colorBin("Reds", agg$dens,
                     bins = 5,
                     na.color = "#ffffff")
    # * bins: La cantidad de categorías de colores.
    # * na.color: El color asignado para las celdas con "NA".
}

info_muestra <- function(muestra) {
    # Muestro la información de una muestra.
    #
    # Args:
    #   muestra: Una muestra, con nombre de especie, densidad y año.

    # Contenido de los popups y etiquetas de los marcadores.
    paste("Investigador: <b>", muestra$researcher, "</b><br>",
          "Especie: <b>", muestra$species, "</b><br>",
          "Impacto: <b>", muestra$impact, "</b><br>",
          "Densidad: <b>", trunc(muestra$dens*10^2)/10^2, "</b><br>",
          "Año: <b>", muestra$year, "</b>")
}

# Funciones para re-dibujar el mapa cuando cambio alguno de los parámetros:
redibujar_grilla <- function(grilla, grosor) {
    # Re-dibuja la capa de la grilla.
    #
    # Args:
    #   grilla: Un objeto SpatialPolygons que representa la grilla.
    #   grosor: Grosor de las líneas de la grilla.

    leafletProxy("mapa") %>%
        clearGroup("Grilla") %>%
        addPolygons(group = "Grilla",
                    color = "black",
                    weight = grosor,
                    opacity = 1,
                    fill = FALSE,
                    data = grilla)
}

redibujar_mapa <- function(lomb.sp, grilla, año_desde, año_hasta, especie, impacto, investigador) {
    # Re-dibuja la capa de la grilla.
    #
    # Args:
    #   lomb.sp: Un objeto SpatialPointsDataFrame que representa las muestras,
    #            con valor de densidad y ubicación geográfica.
    #   grilla: Un objeto SpatialPolygons que representa la grilla.
    #   año_desde: Año mínimo para filtrar las muestras.
    #   año_hasta: Año máximo para filtrar las muestras.
    #   especie: Especie deseada para filtrar las muestras.
    #   impacto: Nivel de impacto deseado para filtrar las muestras.
    #   investigador: Nombre del investigador deseado para filtrar las muestras.

    # Extraigo las muestras que coincidan con el año, la especie, el impacto y
    # el investigador elegidos:
    lomb.sp <- lomb.sp[lomb.sp$year >= año_desde & lomb.sp$year <= año_hasta, ]

    if (especie == "No juveniles") {
        # Filtro para sacar las muestras que no provengan de lombrices
        # juveniles
        lomb.sp <- lomb.sp[lomb.sp$species != "Juveniles", ]

    } else if (especie != "Todos") {
        # Muestras de todas las especies
        lomb.sp <- lomb.sp[lomb.sp$species == especie, ]
    }

    if (impacto != "Todos") {
        lomb.sp <- lomb.sp[lomb.sp$impact == impacto, ]
    }

    if (investigador != "Todos") {
        lomb.sp <- lomb.sp[lomb.sp$researcher == investigador, ]
    }


    agg <- agrupar_muestras(lomb.sp, grilla)

    # Paleta de colores según los datos:
    qpal <- armar_paleta(agg)

    # Re-dibujo el mapa, borrando el mapa de calor y los marcadores
    # anteriores:
    # Agrego la grilla al mapa, pasando también la lista de muestras del año y
    # la especie y el título de la leyenda.
    l <- leafletProxy("mapa") %>%
        clearGroup("Mapa calor") %>%
        clearGroup("Marcadores") %>%
        removeControl("botonCentrar") %>%
        removeControl("leyenda") %>%
        addPolygons(group = "Mapa calor",
                    stroke = FALSE,
                    opacity = 1,
                    fillColor = ~qpal(dens),
                    fillOpacity = 0.5,
                    label = ~as.character(trunc(dens*10^2)/10^2),
                    options = pathOptions(pane = "tilePane"),
                    data = agg) %>%
        addLegend(pal = qpal,
                  values = ~dens,
                  na.label = "Sin muestras",
                  title = paste("Investigador: ", investigador, "<br>Especie: ", especie, "<br>Impacto: ", impacto, "<br>Fecha: ", año_desde, "-", año_hasta, sep = ""),
                  group = "Leyenda",
                  layerId = "leyenda",
                  data = agg) %>%
        addMarkers(group = "Marcadores",
                   popup = info_muestra(lomb.sp),
                   popupOptions = popupOptions(closeButton = FALSE),
                   label = lapply(info_muestra(lomb.sp), HTML),
                   data = lomb.sp) %>%
        addControl(actionButton("zoomer", "", icon = icon("dot-circle-o")),
                   position = "topleft",
                   layerId = "botonCentrar")
}


# Servidor
server <- function(input, output, session) {

    lomb.sp <- importar_datos()

    fig <- importar_figura()
    centro <- gCentroid(fig)@coords # Centro de la figura, para usar en setView

    lomb.sp <- adaptar_datos_espaciales(lomb.sp, fig)

    actualizar_controles(session, lomb.sp)

    grilla <- armar_grilla(fig, 25)

    output$mapa <- renderLeaflet({

        # Límites de la figura, para restringir el desplazamiento en el mapa.
        limites <- bbox(fig)

        # Construyo el mapa
        l <- leaflet(options = leafletOptions(minZoom = 6)) %>%
            addTiles %>%
            setView(lat = centro[[2]], lng = centro[[1]], zoom = 6) %>%
            setMaxBounds(lng1 = limites[[1]],
                         lat1 = limites[[2]],
                         lng2 = limites[[3]],
                         lat2 = limites[[4]])

        # * Control de visibilidad de capas, en donde permite ver solo una capa
        # por año a la vez, y permite mostrar u oculta la grilla
        # * Botón para centrar el mapa en el centro de la figura.
        l <- l %>%
            addLayersControl(overlayGroups = c("Leyenda", "Marcadores"),
                                    position = "topleft",
                                    options = layersControlOptions(collapsed = FALSE))
    })

    # Observo el cambio del deslizador del tamaño de la celda, para
    # re-construir la grilla.
    observeEvent(input$tamaño, {

        # Modifico la grilla con el nuevo tamaño de celda, por lo que obtengo
        # una grilla con distinto número de filas y columnas.
        grilla <<- armar_grilla(fig, input$tamaño)

        # Redibujo la grilla y el mapa de calor usando el nuevo objeto de la
        # grilla.
        redibujar_mapa(lomb.sp, grilla, input$año[[1]], input$año[[2]], input$especie, input$impacto, input$investigador)
        redibujar_grilla(grilla, input$grosor)

    }, ignoreInit = TRUE)

    # Observo los cambios para los controles del año y la especie, para dibujar
    # el mapa de calor correspondiente:
    observeEvent({
        input$año ;
        input$especie ;
        input$impacto ;
        input$investigador
        }, {

        # input$año es un vector de dos elementos con los años desde y hasta
        # del deslizador con rango.
        redibujar_mapa(lomb.sp, grilla, input$año[[1]], input$año[[2]], input$especie, input$impacto, input$investigador)

    }, ignoreInit = TRUE)

    # Observo los cambios en el deslizador del grosor de la grilla, para
    # redibujar la capa de la grilla con el grosor deseado.
    observeEvent(input$grosor, {

        redibujar_grilla(grilla, input$grosor)

    })

    # Observo el botón para centrar el mapa.
    observeEvent(input$zoomer, {
        l <- leafletProxy("mapa") %>%
            setView(lat = centro[[2]], lng = centro[[1]], zoom = input$mapa_zoom)
    })
}
