# Leaflet Grilla Buenos Aires
Dibuja sobre la provincia de Buenos Aires una grilla, para después colorear cada celda según algún dato.

* Origen figuras de Argentina, Provincias y Partidos: http://biogeo.ucdavis.edu/data/diva/adm/ARG_adm.zip

![Captura](https://raw.githubusercontent.com/LeonMarchetti/Leaflet-Grilla-BsAs/master/screenshot.jpg)

## Instalación
Solo hay que clonar el repositorio.

## Ejecución
Abrir el archivo `App.R ` en RStudio y ejecutarlo con `Run App`.

Como se trata de un proyecto que utiliza la librería **Shiny** de R, se sirve el proyecto en un puerto de la máquina, con lo que luego se puede mostrar en una página web al utilizar un `iframe` cuya fuente sea la dirección expuesta por Shiny.

## Características
* Controles:
    * Se puede regular el grosor de las líneas de la grilla, en donde el grosor 0 (transparente) permite mostrar solo el mapa de calor.
    * Se puede regular el tamaño, en kilómetros, de las celdas de la grilla.
    * Control seleccionador para filtrar las muestras por especie.
    * Control deslizante doble para filtrar las muestras entre un año mínimo y otro máximo.
    * Leyenda con la distribución de colores, propia para cada combinación de especie, años y tamaño de celda.
* Marcadores:
    * Se coloca un marcador para cada muestra, indicando el lugar donde se la tomó.
    * Pasando el cursor sobre cada marcador muestra un cartel con los datos de la muestra correspondiente, como la especie, la fecha y la densidad.
    * Se puede alternar la vista de los marcadores de las muestras.
* Grilla:
    * Pasando el cursor sobre una celda con valor de densidad muestra un pequeño cartel con tal valor.

## Notas
* Este proyecto usa una serie de muestras generadas al azar, que abarcan lo más posible de la superficie de la provincia de Buenos Aires.
