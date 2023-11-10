# Importación de los datos ----

## Esperanza de vida ----
library(readr)
EsperanzaVida <- fromJSON(file = "EsperanzaVida.json")
head(EsperanzaVida)

## Cantidad de agua consumida ----
cantidadAgua <- fromJSON(file = "CantidadAgua.json")
head(cantidadAgua)

cantidadAgua <- spread_all(cantidadAgua)


## Calidad del agua ----
calidadAgua <- extract_tables(
    file   = "Cap.3_part2._Libro_blanco_del_agua.pdf", 
    method = "decide", 
    output = "data.frame")