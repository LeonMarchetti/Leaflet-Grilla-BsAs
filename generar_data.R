library(dplyr)

# Data.frame con valores aleatorios
df <- data.frame(species = character(),
                 year = integer(),
                 x = double(),
                 y = double(),
                 dens = integer())

# Cantidad de muestras para generar
cantidad <- 100

# Lista de especies disponibles
# TODO: Agregar más especies
lista_especies <- c("Aporrectodea caliginosa")

# Rango de años
rango_año <- 1988:2013

# Rango de coordenadas:

# Límite rectangular de la provincia de Buenos Aires
# bbox(bsas)
#         min       max
# x -63.39386 -56.66736
# y -41.03542 -33.26014

# min_x <- -63.39386
# max_x <- -56.66736
# min_y <- -41.03542
# max_y <- -33.26014

# Limite rectangular del conjunto de partidos
# bbox(fig)
#         min       max
# x -59.82888 -58.78291
# y -35.25217 -34.38187
min_x <- -59.82888
max_x <- -58.78291
min_y <- -35.25217
max_y <- -34.38187

# Rango de densidades
rango_dens <- 1:200

for (i in 1:cantidad) {
    s <- sample(lista_especies, size = 1)
    a <- sample(rango_año, size = 1)
    x <- runif(1, min = min_x, max = max_x)
    y <- runif(1, min = min_y, max = max_y)
    d <- sample(rango_dens, size = 1)
    df <- add_row(df,
                  species = s,
                  year = a,
                  x = x,
                  y = y,
                  dens = d)
}

View(df)

# Escribir a archivo
write.table(df,
            file = "lombriz-data-rand.csv",
            quote = FALSE,
            sep = ",",
            row.names = FALSE)
