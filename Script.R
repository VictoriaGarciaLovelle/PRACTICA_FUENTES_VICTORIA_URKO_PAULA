# Importación de los datos ----

## Esperanza de vida ----
library(readr)
library(rjson)
EsperanzaVida <- fromJSON(file = "EsperanzaVida.json")
head(EsperanzaVida)

## Cantidad de agua consumida ----
cantidadAgua <- fromJSON(file = "CantidadAgua.json")
head(cantidadAgua)

cantidadAgua <- spread_all(cantidadAgua)


## Calidad del agua ----
install.packages("pdftools")
library(pdftools)
