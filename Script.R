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
calidadAgua <- pdftools::pdf_text("calidadDelAgua.pdf")

calidadAgua
