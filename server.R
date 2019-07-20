library(dplyr)
library(rgeos)
library(rgdal)
library(sp)
library(leaflet)
library(raster)
library(stringr)
library(geosphere)

mostrar <- function(titulo, x) {
    # DEBUG: Función para mostrar una variable por consola
    cat("\n", titulo, "\n")
    print(x)
    cat("\n")
}

# Función de interpolación:
interpol <- function(x) { 
    x / 2 
}

dimensiones_celdas <- function(d, m) {
    # Función que determina la cantidad de filas y columnas debe tener la grilla
    # de la provincia en base al tamaño de esta y el que tendría la celda.
    # d <- tamaño en km que debería tener la celda
    # m <- matriz con las dimensiones de la figura que va a tener la grilla
    
    # Resultado de bbox(bsas):
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
    
    c(filas, columnas)
}

server <- function(input, output) {
    # Datos
    # lomb.data <- read.csv("./lombriz-data.csv")
    lomb.data <- read.csv("./lombriz-data-rand.csv")
    
    # Convierto los datos importados en puntos espaciales.
    lomb.sp <- lomb.data
    coordinates(lomb.sp) <- ~x+y
    
    # ARG_adm1.shp tiene las formas de las provincias.
    argentina <- readOGR(dsn = "./ARG_adm/ARG_adm1.shp", 
                         verbose = FALSE) 
    
    # ARG_adm1.csv tiene los nombres de las provincias para la detección de la
    # forma.
    bsas <- subset(argentina, str_detect(NAME_1, "Buenos Aires")) 
    
    # Enfuerzo los límites de la provincia sobre los puntos
    proj4string(lomb.sp) <- proj4string(bsas)
    
    # Simplifico la figura de la provincia para reducir el tiempo de ejecución.
    bsas <- gSimplify(bsas, 
                      tol = 0.05)
    
    # Borro las muestras que quedan afuera de la provincia
    lomb.sp <- lomb.sp[bsas, ]
    
    # Determino el límite rectangular de la provincia
    # bbox(bsas)
    #         min       max
    # x -63.39386 -56.66736
    # y -41.03542 -33.26014
    e <- extent(bbox(bsas))
    
    # Convierto a objeto raster
    r <- raster(e)
    
    # Divido en grilla de filas x columnas
    dim(r) <- dimensiones_celdas(25, bbox(bsas))
    projection(r) <- crs(proj4string(bsas))
    
    # Agrego el ID de etiqueta a las celdas
    # * Sacando esta linea se saca la grilla
    r <- setValues(r, 1:ncell(r))
    
    # Reconvierto en un archivo de forma para crear un popup del ID de celda
    # para cada polígono
    shape <- rasterToPolygons(r, dissolve = TRUE)
    
    # Recorto las celdas de la grilla que contengan el polígono de la
    # provincia.
    p <- shape[bsas, ]
    
    # Recorto el perímetro de la grilla para coincidir con el polígono de la
    # provincia
    map <- gIntersection(p, bsas, 
                         byid = TRUE, 
                         drop_lower_td = TRUE)
    
    # Agrupa cada muestra en la grilla de la provincia, y calcula el promedio
    # en cada una
    agg <- aggregate(x = lomb.sp["dens"],
                     by = map, 
                     FUN = mean)
    # * con `FUN = max` muestra una celda con el valor máximo 263 que aparece 
    #   en el archivo, así que hago `FUN = mean` para el promedio para cada 
    #   celda.
    
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
    
    # Muestro la lista de celdas y los promedios de densidad en cada una
    # después de sumar los valores interpolados.
    # mostrar("agg$dens", agg$dens)
    
    # Creo la paleta de colores, según los valores de la densidad de los 
    # datos.
    # * na.color significa el color asignado para las celdas con "NA".
    qpal <- colorBin("Reds", agg$dens,
                     bins = 5,
                     na.color = "#ffffff")
    
    output$mapa <- renderLeaflet({
        # Creación del mapa, usando la agregación anterior como fuente de datos.
        l <- leaflet(agg) %>%
            addTiles %>%
            addLegend(values = ~dens, 
                      pal = qpal, 
                      title = "Densidad",
                      group = "Grilla") %>%
            addMarkers(data = lomb.sp,
                       popup = paste("Especie: <b>", lomb.sp$species, "</b><br>",
                                     "Densidad: <b>", lomb.sp$dens, "</b><br>",
                                     "Año: <b>", lomb.sp$year, "</b>"),
                       label = lapply(paste("Especie: <b>", lomb.sp$species, "</b><br>",
                                            "Densidad: <b>", lomb.sp$dens, "</b><br>",
                                            "Año: <b>", lomb.sp$year, "</b>"),
                                      htmltools::HTML),
                       popupOptions = popupOptions(closeButton = FALSE),
                       group = "Marcadores") %>% 
            addLayersControl(overlayGroups = c("Grilla", "Marcadores"),
                             options = layersControlOptions(collapsed = FALSE),
                             position = "topleft")
        # * addLayersControl agrega un control de capas, para poder alternar la
        #   vista de los marcadores y de la vista de la grilla.
    })
    
    # Observo los cambios en el deslizador del grosor de la grilla, para redibujar
    # la capa de la grilla con el grosor deseado.
    observeEvent(input$grosor, {
        leafletProxy("mapa", data = agg) %>%
            clearGroup("Grilla") %>%
            addPolygons(stroke = TRUE,
                        opacity = 1,
                        fillOpacity = 0.5,
                        smoothFactor = 0.5,
                        color = "black",
                        fillColor = ~qpal(dens),
                        label = ~as.character(dens),
                        weight = input$grosor,
                        group = "Grilla")
    })
}
