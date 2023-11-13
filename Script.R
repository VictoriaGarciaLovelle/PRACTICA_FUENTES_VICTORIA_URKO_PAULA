# Importación de los datos ----
install.packages("pdftools")
library(pdftools)
library(readr)
library(rjson)

## Esperanza de vida ----
EsperanzaVida <- fromJSON(file = "EsperanzaVida.json")
head(EsperanzaVida)

## Cantidad de agua consumida ----
cantidadAgua <- fromJSON(file = "CantidadAgua.json")
head(cantidadAgua)

cantidadAgua <- spread_all(cantidadAgua)


## Calidad del agua ----
calidadAgua <- "Cap.3_part2._Libro_blanco_del_agua.pdf"
calidadAgua <- pdf_text(calidadAgua)
str(calidadAgua)
