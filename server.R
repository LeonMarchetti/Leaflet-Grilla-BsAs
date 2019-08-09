library(dplyr)
library(rgeos)
library(rgdal)
library(sp)
library(leaflet)
library(raster)
library(stringr)
library(geosphere)
library(htmltools)

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

    # lomb.data <- read.csv("./lombriz-data.csv")
    lomb.data <- read.csv("./lombriz-data-rand.csv")

    # Convierto los datos importados en puntos espaciales.
    lomb.sp <- lomb.data
    coordinates(lomb.sp) <- ~x+y

    return(lomb.sp)
}

importar_figura <- function() {
    # Importo de un archivo la figura sobre donde hacer la grilla.
    #
    # Returns:
    #   Un objeto SpatialPolygons que representa la figura importada.

    # ARG_adm2.shp tiene las formas de los partidos.
    argentina <- readOGR(dsn = "./ARG_adm/ARG_adm2.shp", verbose = FALSE)

    # ARG_adm2.csv tiene los nombres de los partidos para la detección de la
    # forma.
    bsas <- subset(argentina, str_detect(NAME_1, "Buenos Aires"))

    # Lista de los partidos (de la provincia de Buenos Aires) que quiero unir:
    partidos <- c(
        "General Las Heras",
        "General Rodríguez",
        "Luján",
        "Mercedes",
        "Navarro"
    )

    # Busco en
    for (nombre_partido in partidos) {
        figura_partido <- subset(bsas, str_detect(NAME_2, nombre_partido))
        if (is.null(f)) f <- figura_partido
        else            f <- rbind(f, figura_partido)
    }

    # Disuelvo las lineas interiores de los partidos unidos.
    f <- aggregate(f, dissolve=TRUE)

    # Simplifico la figura para reducir el tiempo de ejecución.
    # gSimplify(f, tol = 0.05)

    return(f)
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

    # Borro las muestras que quedan afuera de la figura
    lomb.sp <- lomb.sp[fig, ]

    return(lomb.sp)
}

actualizar_controles <- function(session, lomb.sp) {
    # Actualizo los controles, cambiando los valores límites o las elecciones
    # posibles, usando los datos importados. Los controles son del framework de
    # Shiny.
    #
    # Args:
    #   session: Objeto sesión de Shiny
    #   lomb.sp: Un objeto SpatialPointsDataFrame que representa las muestras,
    #            con valor de densidad y ubicación geográfica.

    # Defino los valores posibles para el select de la especie:
    updateSelectInput(session, inputId = "especie",
                      choices = sort(unique(lomb.sp$species)))

    # Defino el rango para el deslizador del año, los años mínimo y máximo en
    # la muestra pasan a ser los límites del deslizador:
    min_año = min(lomb.sp$year)
    max_año = max(lomb.sp$year)
    updateSliderInput(session, inputId = "año", min = min_año, max = max_año,
                      value = c(min_año, max_año))
}

dimensiones_celdas <- function(d, m) {
    # Función que determina la cantidad de filas y columnas debe tener la
    # grilla de la figura en base al tamaño de esta y el que tendría la
    # celda.
    #
    # Args:
    #   d: Tamaño en km que debería tener la celda.
    #   m: Matriz con las dimensiones de la figura que va a tener la grilla.
    #
    # Returns:
    #   Un par (filas, columnas) con la cantidad de filas y columnas de la
    #   grilla.

    # Resultado de bbox(fig):
    min_x <- m[[1]]
    min_y <- m[[2]]
    max_x <- m[[3]]
    max_y <- m[[4]]

    # Puntos de las esquinas:
    esq_ii <- c(min_x, min_y)
    #esq_id <- c(max_x, min_y)
    esq_si <- c(min_x, max_y)
    esq_sd <- c(max_x, max_y)

    # Distancia de cada punto en kilómetros
    distHorizontal <- distHaversine(esq_si, esq_sd) / 1000
    distVertical <- distHaversine(esq_ii, esq_si) / 1000

    # División de la distancia con el tamaño deseado de la celda:
    filas <- distVertical %/% d
    columnas <- distHorizontal %/% d

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
    #   Un objeto SpatialPolygons que representa la grilla sobre la figura

    # Determino el límite rectangular de la figura
    # bbox(fig)
    #         min       max
    # x -59.82888 -58.78291
    # y -35.25217 -34.38187
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

    # Recorto las celdas de la grilla que contengan el polígono de la figura
    p <- shape[fig, ]

    # Recorto el perímetro de la grilla para coincidir con el polígono de la
    # figura
    gIntersection(p, fig, byid = TRUE, drop_lower_td = TRUE)
}

interpol <- function(x) {
    # Función de interpolación:
    x / 2
}

agrupar_muestras <- function(lomb.sp, grilla) {
    # Agrupa cada muestra en la grilla de la figura, y calcula el promedio
    # en cada una de las celdas. Luego calcula un valor de interpolación para
    # cada celda y lo suma a cada celda vecina.
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
    agg <- aggregate(lomb.sp, grilla, function(x) mean(as.numeric(x)))

    # Transformo los valores no existentes en 0.
    # agg$dens[is.na(agg$dens)] <- 0

    # Transformo la projección de las coordenadas de la agregación.
    # (No es necesario?)
    # agg <- spTransform(agg, CRS("+init=epsg:4326"))

    # Muestro la lista de celdas y los promedios de densidad en cada una.
    # mostrar("agg$dens", agg$dens)

    # Calculo los valores a sumar para cada celda. Como la agregación me
    # devuelve una lista para representar la grilla, entonces voy a calcular
    # la posición de la siguiente celda según su índice y la cantidad de filas
    # y columnas.
    list_interpol <- rep(0, length(agg))

    # Lista de vecinos de todos los polígonos
    list.vec <- gTouches(grilla, byid = TRUE, returnDense = FALSE)

    for (i in 1:length(agg)) {

        # Si no hay valor en la lista inicial, entonces no calculo nada:
        if (!is.na(agg$dens[[i]])) {

            # Itero sobre los vecinos del polígono "i"
            for (v in list.vec[[i]]) {
                list_interpol[[v]] <- list_interpol[[v]] + interpol(agg$dens[[i]])
            }
        }
    }

    # Sumo los valores interpolados a la agregación:
    for (i in 1:length(agg)) {
        if (list_interpol[[i]] != 0 & is.na(agg$dens[[i]]) ) {
            agg$dens[[i]] <- 0
        }
        agg$dens[[i]] <- agg$dens[[i]] + list_interpol[[i]]
    }

    # DEBUG: Muestro la lista de celdas y los promedios de densidad en cada una
    # después de sumar los valores interpolados.
    # mostrar("agg$dens", agg$dens)

    return(agg)
}

armar_paleta <- function(agg) {
    # Creo la paleta de colores, según los valores de la densidad de las
    # celdas de la grilla.
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
    # * na.color significa el color asignado para las celdas con "NA".
}

info_muestra <- function(muestra) {
    # Muestro la información de una muestra.
    #
    # Args:
    #   muestra: Una muestra, con nombre de especie, densidad y año.

    # Contenido de los popups y etiquetas de los marcadores.
    paste("Especie: <b>", muestra$species, "</b><br>",
          "Densidad: <b>", muestra$dens, "</b><br>",
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

redibujar_mapa <- function(lomb.sp, grilla, año_desde, año_hasta, especie) {
    # Re-dibuja la capa de la grilla.
    #
    # Args:
    #   lomb.sp: Un objeto SpatialPointsDataFrame que representa las muestras,
    #            con valor de densidad y ubicación geográfica.
    #   grilla: Un objeto SpatialPolygons que representa la grilla.
    #   año_desde: Año mínimo para filtrar las muestras.
    #   año_hasta: Año máximo para filtrar las muestras.
    #   especie: Especie deseada para filtrar las muestras.

    # Extraigo las muestras que coincidan con el año y la especie elegida:
    lomb.sp.año <- lomb.sp[lomb.sp$year >= año_desde &
                           lomb.sp$year <= año_hasta, ]
    lomb.sp.año.especie <- lomb.sp.año[lomb.sp.año$species == especie, ]

    agg <- agrupar_muestras(lomb.sp.año.especie, grilla)

    # Paleta de colores según los datos:
    qpal <- armar_paleta(agg)

    # Re-dibujo el mapa, borrando el mapa de calor y los marcadores
    # anteriores:
    # Agrego la grilla al mapa, pasando también la lista de muestras del año y
    # la especie y el título de la leyenda.
    l <- leafletProxy("mapa") %>%
        clearGroup("Mapa calor") %>%
        clearGroup("Marcadores") %>%
        clearControls %>%
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
                  title = paste("Densidad:", paste(especie, " [", año_desde,
                                                   "-", año_hasta, "]",
                                                   sep = "")),
                  group = "Mapa calor",
                  data = agg) %>%
        addMarkers(group = "Marcadores",
                   popup = info_muestra(lomb.sp.año.especie),
                   popupOptions = popupOptions(closeButton = FALSE),
                   label = lapply(info_muestra(lomb.sp.año.especie), HTML),
                   data = lomb.sp.año.especie)
}


# Servidor
server <- function(input, output, session) {

    lomb.sp <- importar_datos()
    fig <- importar_figura()
    lomb.sp <- adaptar_datos_espaciales(lomb.sp, fig)

    actualizar_controles(session, lomb.sp)

    # Queda puesto acá para inicializar la grilla antes de que se ejecuten los
    # observadores de los controles. Tiene que coincidir el tamaño con el
    # puesto en ui.R.
    grilla <- armar_grilla(fig, 10)

    output$mapa <- renderLeaflet({

        # Centro de la figura, para usar en setView, ya que no funciona si no
        # se indican las coordenadas.
        centro <- gCentroid(fig)@coords

        # Construyo el mapa
        l <- leaflet() %>% addTiles %>%
            setView(lat = centro[[2]], lng = centro[[1]], zoom = 9)

        # Control de visibilidad de capas, en donde permite ver solo una capa
        # por año a la vez, y permite mostrar u oculta la grilla
        l <- l %>% addLayersControl(overlayGroups = c("Marcadores"),
                                    position = "topleft",
                                    options = layersControlOptions(
                                        collapsed = FALSE))
    })

    # Observo el cambio del deslizador del tamaño de la celda, para
    # re-construir la grilla.
    observeEvent(input$tamaño, {

        # Modifico la grilla con el nuevo tamaño de celda, por lo que obtengo
        # una grilla con distinto número de filas y columnas.
        grilla <<- armar_grilla(fig, input$tamaño)

        # Redibujo la grilla y el mapa de calor usando el nuevo objeto de la
        # grilla.
        redibujar_mapa(lomb.sp, grilla, input$año[[1]], input$año[[2]],
                       input$especie)
        redibujar_grilla(grilla, input$grosor)

    }, ignoreInit = TRUE)

    # Observo los cambios para los controles del año y la especie, para dibujar
    # el mapa de calor correspondiente:
    observeEvent({input$año ; input$especie}, {

        # input$año es un vector de dos elementos con los años desde y hasta
        # del deslizador con rango.
        redibujar_mapa(lomb.sp, grilla, input$año[[1]], input$año[[2]],
                       input$especie)

    }, ignoreInit = TRUE)

    # Observo los cambios en el deslizador del grosor de la grilla, para
    # redibujar la capa de la grilla con el grosor deseado.
    observeEvent(input$grosor, {

        redibujar_grilla(grilla, input$grosor)

    })
}
