# Importación de los datos ----
install.packages("pdftools")
library(pdftools)
library(glue)
library(readr)
library(rjson)
library(tidyverse)
library(tidyjson)

## Esperanza de vida ----
esperanzaDeVida <- fromJSON(file = "EsperanzaVida.json")

esperanzaVida<- spread_all(esperanzaDeVida)
esperanzaVida



str(esperanza[[1]]["Nombre"], na.rm=TRUE)

## Cantidad de agua consumida ----
cantidadDeAgua <- fromJSON(file = "CantidadAgua.json")

cantidadAgua <- spread_all(cantidadDeAgua)
cantidadAgua

## Calidad del agua ----
url <- "https://www.chj.es/es-es/medioambiente/planificacionhidrologica/Documents/Plan%20de%20Recuperaci%C3%B3n%20del%20J%C3%BAcar/Cap.3_part2._Libro_blanco_del_agua.pdf"
urls <- glue(url)
pdf_names <- glue("report_Cap.3_part2._Libro_blanco_del_agua.pdf")
walk2(urls, pdf_names, download.file, mode = "wb")
raw_text <- map(pdf_names, pdf_text)
str (raw_text)
