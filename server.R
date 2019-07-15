library(dplyr)
library(rgeos)
library(rgdal)
library(sp)
library(leaflet)
library(raster)
library(stringr)

server <- function(input, output) {
    output$mapa <- renderLeaflet({
        
        # Datos
        lomb.data <- read.csv("./lombriz-data.csv")
        
        # Obtengo todos los años de las muestras:
        años <- as.character(unique(lomb.data$year))
        
        # Convierto los datos importados en puntos espaciales.
        lomb.sp <- lomb.data
        coordinates(lomb.sp) <- ~y+x
        
        # ARG_adm1.shp tiene las formas de las provincias.
        argentina <- readOGR(dsn = "./ARG_adm/ARG_adm1.shp") 
        
        # ARG_adm1.csv tiene los nombres de las provincias para la detección de la
        # forma.
        bsas <- subset(argentina, str_detect(NAME_1, "Buenos Aires")) 
        
        # Enfuerzo los límites de la provincia sobre los puntos
        proj4string(lomb.sp) <- proj4string(bsas)
        
        # Simplifico la figura de la provincia para reducir el tiempo de ejecución.
        bsas <- gSimplify(bsas, 
                          tol = 0.05)
        
        # Determino el límite rectangular de la provincia
        # bbox(bsas)
        #         min       max
        # x -63.39386 -56.66736
        # y -41.03542 -33.26014
        e <- extent(bbox(bsas))
        
        # Convierto a objeto raster
        r <- raster(e)
        
        # Divido en grilla de filas x columnas
        # TODO: Calcular filas y columnas según distancia deseada y el tamaño de la provincia (usando el resultado de bbox).
        dim(r) <- c(25, 25)
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
        # agg <- spTransform(agg, CRS("+init=epsg:4326"))
        
        # Creo la paleta de colores, según los valores de la densidad de los 
        # datos.
        # * na.color significa el color asignado para las celdas con "NA".
        qpal <- colorBin("Reds", agg$dens, 
                         bins = 5,
                         na.color = "#ffffff")
        
        # Muestro la lista de celdas y los promedios de densidad en cada una.
        cat("\n", "agg$dens", "\n")
        print(agg$dens)
        cat("\n")
        
        # Creación del mapa, usando la agregación anterior como fuente de datos.
        l <- leaflet(agg) %>%
            addTiles %>%
            addPolygons(stroke = TRUE,
                        opacity = 1,
                        fillOpacity = 0.5, 
                        smoothFactor = 0.5,
                        color = "black",
                        fillColor = ~qpal(dens),
                        weight = 0.5,
                        group = "Grilla") %>%
            addLegend(values = ~dens, 
                      pal = qpal, 
                      title = "Densidad",
                      group = "Grilla")
        
        # Agrego una capa de marcadores por cada año:
        for (año in años) {
            # Obtengo del data frame las muestras del año en particular.
            datos <- lomb.sp[lomb.sp$year == año, ]
            
            # Armo una capa de marcadores, con las muestras del año.
            l = l %>% addMarkers(data = datos,
                                 popup = paste("Especie: <b>", datos$species, "</b><br>",
                                               "Densidad: <b>", datos$dens, "</b><br>",
                                               "Año: <b>", datos$year, "</b>"),
                                 label = lapply(paste("Especie: <b>", datos$species, "</b><br>",
                                                      "Densidad: <b>", datos$dens, "</b><br>",
                                                      "Año: <b>", datos$year, "</b>"),
                                                htmltools::HTML),
                                 popupOptions = popupOptions(closeButton = FALSE),
                                 group = año)
        }
        
        # Control de capas, para poder alternar la vista de cada capa anual de
        # marcadores, y también alternar la vista de la grilla.
        l %>% addLayersControl(overlayGroups = c(años, "Grilla"),
                               options = layersControlOptions(collapsed = FALSE))
    })
}
