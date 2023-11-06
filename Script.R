# Importación de los datos

## Calidad de las aguas de consumo humano
library(rjson)

calidadAgua <- fromJSON(file = "calidad-de-las-aguas-de-consumo-humano.json")
head (calidadAgua)

## Calidad de vida
library(readr)
calidad_de_vida <- read_delim("calidad_de_vida.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)



