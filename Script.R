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



str(esperanzaVida[[1]]["Nombre"], na.rm=TRUE)

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
raw_text[[1]][17] #Permite observar el segmento que nos interesa

#
clean_table <- function(table){
  table <- str_split(table, "\n", simplify = TRUE)
  country_name <- table[1, 1] %>% 
    stringr::str_squish() %>% 
    stringr::str_extract(".+?(?=\\sTotal)")
  table_start <- stringr::str_which(table, "Prevalence of diabetes")
  table_end <- stringr::str_which(table, "National response to diabetes")
  table <- table[1, (table_start +1 ):(table_end - 1)]
  table <- str_replace_all(table, "\\s{2,}", "|")
  text_con <- textConnection(table)
  data_table <- read.csv(text_con, sep = "|")
  colnames(data_table) <- c("Condition", "Males", "Females", "Total")
  dplyr::mutate(data_table, Country = country_name)
}