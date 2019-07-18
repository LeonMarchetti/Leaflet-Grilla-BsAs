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
        lomb.data <- read.csv("./lombriz-data-rand.csv")
        
        # Convierto en puntos espaciales.
        lomb.sp <- lomb.data
        coordinates(lomb.sp) <- ~x+y
        
        # ARG_adm1.shp tiene las formas de las provincias.
        argentina <- readOGR(dsn = "./ARG_adm/ARG_adm1.shp") 
        
        # ARG_adm1.csv tiene los nombres de las provincias para la detección de la
        # forma.
        bsas <- subset(argentina, str_detect(NAME_1, "Buenos Aires")) 
        
        # Enforzar los límites de la provincia sobre los puntos
        proj4string(lomb.sp) <- proj4string(bsas)
        
        # Simplifico la figura de la provincia para reducir el tiempo de ejecución.
        bsas <- gSimplify(bsas, 
                          tol = 0.05)
        
        # Determinar el límite rectangular de la provincia
        # bbox(bsas)
        #         min       max
        # x -63.39386 -56.66736
        # y -41.03542 -33.26014
        e <- extent(bbox(bsas))
        
        # Convertir a objeto raster
        r <- raster(e)
        
        # Dividir en grilla de 25 x 25
        # TODO: Calcular filas y columnas según distancia deseada y el tamaño de la provincia (usando el resultado de bbox).
        dim(r) <- c(25, 25)
        projection(r) <- crs(proj4string(bsas))
        
        # Agregar ID de etiqueta a las celdas
        # * Sacando esta linea se saca la grilla
        r <- setValues(r, 1:ncell(r))
        
        # Reconvertir en un archivo de forma para crear un popup del ID de celda
        # para cada polígono
        shape <- rasterToPolygons(r, dissolve = TRUE)
        
        # Recortar las celdas de la grilla que contengan el polígono de la
        # provincia.
        p <- shape[bsas, ]
        
        # Recortar el perímetro de la grilla para coincidir con el polígono de la
        # provincia
        map <- gIntersection(p, bsas, 
                             byid = TRUE, 
                             drop_lower_td = TRUE)
        
        # 
        agg <- aggregate(x = lomb.sp["dens"], 
                         by = map, 
                         FUN = mean)
        agg$dens[is.na(agg$dens)] <- 0
        agg <- spTransform(agg, CRS("+init=epsg:4326"))
        qpal <- colorBin("Reds", agg$dens, 
                         bins = 5)
        # * con `FUN = max` muestra una celda con el valor máximo 263 que aparece en
        # el archivo.
        # * así que hago `FUN = mean` para el promedio para cada celda.
        
        cat("\n", "agg$dens", "\n")
        print(agg$dens) # Muestra la lista de celdas y los promedios de densidad 
                        # para cada una.
        cat("\n")
        
        leaflet(agg) %>%
            addTiles %>%
            addPolygons(stroke = TRUE,
                        opacity = 1,
                        fillOpacity = 0.5, 
                        smoothFactor = 0.5,
                        color = "black",
                        fillColor = ~qpal(dens),
                        weight = 0.5) %>%
            addLegend(values = ~dens, 
                      pal = qpal, 
                      title = "Densidad")
    })
}
