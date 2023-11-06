# Importación de los datos ----

## Calidad de las aguas de consumo humano ----
#library(rjson)

#calidadAgua <- fromJSON(file = "calidad-de-las-aguas-de-consumo-humano.json")
#head (calidadAgua)

#library(tidyjson)
#calidadAgua <- spread_all(calidadAgua)


## Esperanza de vida ----
library(readr)
EsperanzaVida <- read_delim("EsperanzaVida.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

## Cantidad de agua consumida ----
cantidadAgua <- fromJSON(file = "CantidadAgua.json")
head(cantidadAgua)

cantidadAgua <- spread_all(cantidadAgua)
