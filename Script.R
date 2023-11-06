# Importación de los datos ----

## Calidad de las aguas de consumo humano ----
library(rjson)

calidadAgua <- fromJSON(file = "calidad-de-las-aguas-de-consumo-humano.json")
head (calidadAgua)

library(tidyjson)
calidadAgua <- spread_all(calidadAgua)
View(calidadAgua)

## Calidad de vida ----
library(readr)
calidadVida <- read_delim("calidad_de_vida.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

## Cantidad de agua consumida ----
cantidadAgua <- fromJSON(file = "CantidadAgua.json")
head(cantidadAgua)

cantidadAgua <- spread_all(cantidadAgua)
View(cantidadAgua)
