library(dplyr)
library(rgeos)
library(rgdal)
library(sp)
library(leaflet)
library(raster)
library(stringr)
library(geosphere)

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

importar_provincia <- function(nombre_prov) {
    # Importo del archivo de la figura del país una provincia en particular.
    #
    # Args:
    #   nombre_prov: El nombre de la provincia a importar.
    #
    # Returns:
    #   Un objeto SpatialPolygons que representa La figura de la provincia.

    # ARG_adm1.shp tiene las formas de las provincias.
    argentina <- readOGR(dsn = "./ARG_adm/ARG_adm1.shp",
                         verbose = FALSE)

    # ARG_adm1.csv tiene los nombres de las provincias para la detección de la
    # forma.
    bsas <- subset(argentina, str_detect(NAME_1, nombre_prov))

    # Simplifico la figura de la provincia para reducir el tiempo de ejecución.
    gSimplify(bsas, tol = 0.05)
}

adaptar_datos_espaciales <- function(lomb.sp, prov) {
    # Adapto el data frame espacial con los datos a la figura de la provincia.
    #
    # Args:
    #   lomb.sp: El data frame espacial (SpatialPointsDataFrame).
    #   prov: La figura (SpatialPolygons) de la provincia.
    #
    # Returns:
    #   El data frame espacial modificado.

    # Enfuerzo los límites de la provincia sobre los puntos
    proj4string(lomb.sp) <- proj4string(prov)

    # Borro las muestras que quedan afuera de la provincia
    lomb.sp <- lomb.sp[prov, ]

    return(lomb.sp)
}

dimensiones_celdas <- function(d, m) {
    # Función que determina la cantidad de filas y columnas debe tener la
    # grilla de la provincia en base al tamaño de esta y el que tendría la
    # celda.
    #
    # Args:
    #   d: Tamaño en km que debería tener la celda.
    #   m: Matriz con las dimensiones de la figura que va a tener la grilla.
    #
    # Returns:
    #   Un par (filas, columnas) con la cantidad de filas y columnas de la
    #   grilla.

    # Resultado de bbox(prov):
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

armar_grilla <- function(prov) {
    # Armo la grilla, que se trata de un conjunto de polígonos cuadrados,
    # siguiendo el contorno de la provincia.
    #
    # Args:
    #   prov: Figura (SpatialPolygons) de la provincia donde armar la grilla.
    #
    # Returns:
    #   Un objeto SpatialPolygons que representa la grilla sobre la provincia.

    # Determino el límite rectangular de la provincia
    # bbox(bsas)
    #         min       max
    # x -63.39386 -56.66736
    # y -41.03542 -33.26014
    e <- extent(bbox(prov))

    # Convierto a objeto raster
    r <- raster(e)

    # Divido en grilla de filas x columnas
    dim(r) <- dimensiones_celdas(25, bbox(prov))
    projection(r) <- crs(proj4string(prov))

    # Agrego el ID de etiqueta a las celdas
    # * Sacando esta linea se saca la grilla
    r <- setValues(r, 1:ncell(r))

    # Reconvierto en un archivo de forma para crear un popup del ID de celda
    # para cada polígono
    shape <- rasterToPolygons(r, dissolve = TRUE)

    # Recorto las celdas de la grilla que contengan el polígono de la
    # provincia.
    p <- shape[prov, ]

    # Recorto el perímetro de la grilla para coincidir con el polígono de la
    # provincia
    gIntersection(p, prov,
                  byid = TRUE,
                  drop_lower_td = TRUE)
}

interpol <- function(x) {
    # Función de interpolación:
    x / 2
}

agrupar_muestras <- function(lomb.sp, map) {
    # Agrupa cada muestra en la grilla de la provincia, y calcula el promedio
    # en cada una de las celdas.
    #
    # Args:
    #   lomb.sp: Un objeto SpatialPointsDataFrame que representa las muestras,
    #   con valor de densidad y ubicación geográfica.
    #   map: Un objeto SpatialPolygons que representa la grilla.
    #
    # Returns:
    #   Un objeto SpatialPolygonsDataFrame que representa a las muestras
    #   agrupadas en cada celda de la grilla y con un valor de densidad
    #   promedio calculado para cada celda.

    agg <- aggregate(x = lomb.sp["dens"],
                     by = map,
                     FUN = mean)

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
    list.vec <- gTouches(map, byid = TRUE, returnDense = FALSE)

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
    # Creo la paleta de colores, según los valores de la densidad de los
    # celdas de la grilla.
    #
    # Args:
    #   agg: Un objeto SpatialPolygonsDataFrame, que representa la grilla con
    #   las muestras agrupadas en cada celda.
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

agregar_grilla <- function(l, agg, lomb.sp, titulo) {
    # Agrega al mapa leaflet la grilla con el mapa de calor. También agrega los
    # marcadores sobre la posición de las muestras.
    #
    # Args:
    #   l: Un objeto Leaflet que representa el mapa sobre el cual dibujar la
    #   grilla.
    #   agg: Un objeto SpatialPolygonsDataFrame, que representa la grilla con
    #   las muestras agrupadas en cada celda.
    #   lomb.sp: Un objeto SpatialPointsDataFrame que representa las muestras,
    #   con valor de densidad y ubicación geográfica.
    #   titulo: Título de la leyenda, que identifica sobre cuales muestras
    #   representa la paleta de colores.
    #
    # Returns:
    #   El objeto Leaflet modificado.

    qpal <- armar_paleta(agg)

    l <- l %>%
        addPolygons(group = "Mapa calor",
                    stroke = FALSE,
                    opacity = 1,
                    fillColor = ~qpal(dens),
                    fillOpacity = 0.5,
                    label = ~as.character(dens),
                    options = pathOptions(pane = "tilePane"),
                    data = agg) %>%
        addLegend(pal = qpal,
                  values = ~dens,
                  title = paste("Densidad:", titulo),
                  group = "Mapa calor",
                  data = agg) %>%
        addMarkers(group = "Marcadores",
                   popup = info_muestra(lomb.sp),
                   popupOptions = popupOptions(closeButton = FALSE),
                   label = lapply(info_muestra(lomb.sp), htmltools::HTML),
                   data = lomb.sp)
    # TODO: Arreglar leyenda, que cuando revelo el mapa de calor con el control de capas muestra las leyendas de otras selecciones anteriores.
}

server <- function(input, output, session) {

    lomb.sp <- importar_datos()
    bsas <- importar_provincia("Buenos Aires")
    lomb.sp <- adaptar_datos_espaciales(lomb.sp, bsas)

    # Defino los valores posibles para los select:
    updateSelectInput(session, inputId = "especie",
                      choices = sort(unique(lomb.sp$species)))

    updateSelectInput(session, inputId = "año",
                      choices = sort(unique(lomb.sp$year)))

    map <- armar_grilla(bsas)

    output$mapa <- renderLeaflet({

        # Centro de la provincia, para usar en setView, ya que no funciona si
        # no se indican las coordenadas.
        centro <- gCentroid(bsas)@coords

        # Construyo el mapa
        l <- leaflet() %>%
            addTiles %>%
            setView(lat = centro[[2]], lng = centro[[1]], zoom = 6)

        # Control de visibilidad de capas, en donde permite ver solo una capa
        # por año a la vez, y permite mostrar u oculta la grilla
        l <- l %>% addLayersControl(overlayGroups = c("Marcadores", "Mapa calor"),
                                    position = "topleft",
                                    options = layersControlOptions(
                                        collapsed = FALSE))
    })

    # Observo los cambios para los controles del año y la especie, para dibujar
    # el mapa de calor correspondiente:
    observeEvent({input$año ; input$especie}, {

        # Extraigo las muestras que coincidan con el año y la especie elegida:
        lomb.sp.año <- lomb.sp[lomb.sp$year == input$año, ]
        lomb.sp.año.especie <- lomb.sp.año[lomb.sp.año$species == input$especie, ]

        agg <- agrupar_muestras(lomb.sp.año.especie, map)

        # Re-dibujo el mapa, borrando el mapa de calor y los marcadores
        # anteriores:
        l <- leafletProxy("mapa") %>%
            clearGroup("Mapa calor") %>%
            clearGroup("Marcadores") %>%
            clearControls

        # Agrego la grilla al mapa, pasando también la lista de muestras del
        # año y la especie y el título de la leyenda
        l <- agregar_grilla(l, agg, lomb.sp.año.especie,
                            paste(input$especie, "-", input$año, sep = ""))
    }, ignoreInit = TRUE)

    # Observo los cambios en el deslizador del grosor de la grilla, para
    # redibujar la capa de la grilla con el grosor deseado.
    observeEvent(input$grosor, {
        leafletProxy("mapa") %>%
            clearGroup("Grilla") %>%
            addPolygons(group = "Grilla",
                        color = "black",
                        weight = input$grosor,
                        opacity = 1,
                        fill = FALSE,
                        data = map)
    })
}
