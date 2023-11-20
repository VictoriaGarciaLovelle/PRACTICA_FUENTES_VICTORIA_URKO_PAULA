# Importación de los datos ----
#install.packages("pdftools")
library(pdftools)
library(glue)
library(readr)
library(rjson)
library(tidyverse)
library(tidyjson)
library(dplyr)
library(stringr)
library(purrr)

## Esperanza de vida ----
EspVidaJson <- fromJSON(file = "EsperanzaVida.json")

esperanzaVida <- spread_all(EspVidaJson)
head(esperanzaVida)

tibble1<-esperanzaVida %>% 
  gather_object %>%  #para cada atributo me dice de qué se trata
  json_types %>% 
  count(name, type)

tibble1

data_meta_values <- tibble1 %>%
  filter(name %in% c("MetaData", "Data")) %>%
  select(name, type, n)

data_meta_values$Data

for (i in seq_len(nrow(data_meta_values))) {
  name <- data_meta_values$name[i]
  data <- lapply(esperanzaVida[[name]][i], fromJSON)
  cat("Contenido de", name, ":\n")
  print(data)
}

# Así puedo acceder a la comunidad autónoma
esperanzaVida[[6]][[1]][["MetaData"]][[1]][["Nombre"]]
#Asi puedo acceder al año
esperanzaVida[[6]][[1]][["Data"]][[6]][["Anyo"]]
#Así puedo acceder al valor de la esperanza de vida
esperanzaVida[[6]][[1]][["Data"]][[1]][["Valor"]]

## Cantidad de agua consumida ----
cantidadDeAgua <- fromJSON(file = "CantidadAgua.json")

cantidadAgua <- spread_all(cantidadDeAgua)
View(cantidadAgua)

## Calidad del agua ----
url <- "https://www.chj.es/es-es/medioambiente/planificacionhidrologica/Documents/Plan%20de%20Recuperaci%C3%B3n%20del%20J%C3%BAcar/Cap.3_part2._Libro_blanco_del_agua.pdf"
urls <- glue(url)
pdf_names <- glue("report_Cap.3_part2._Libro_blanco_del_agua.pdf")
walk2(urls, pdf_names, download.file, mode = "wb")
raw_text <- map(pdf_names, pdf_text)
str(raw_text)
raw_text[[1]][17] #Permite observar el segmento que nos interesa

# Empezar poco a poco viendo lo que vamos obteniendo...
raw_text %>% 
  str_split(., "\n", simplify = TRUE) 


clean_table <- function(table){
  table <- str_split(table, "\n", simplify = TRUE)
  calidadAgua <- table[8, 13] %>% 
    stringr::str_squish() %>% 
    stringr::str_extract(".+?(?=\\sTotal)")
  table_start <- stringr::str_which(table, "Comunidad Autónoma")
  table_end <- stringr::str_which(table, "Aguas SCF")
  #table <- table[1, (table_start +1 ):(table_end - 1)]
  table <- str_replace_all(table, "\\s{2,}", "|")
  text_con <- textConnection(table)
  data_table <- read.csv(text_con, sep = "|", row.names = NULL)
  colnames(data_table) <- c("Num. de Municipios", "Zonas de baño","Puntos de muestreo", "Aguas 2", "Aguas 1", "Aguas 0", "Aguas SCF")
  dplyr::mutate(data_table, calidad = calidadAgua)
}

calidadDelAgua <- map_df(raw_text, clean_table)
calidadDelAgua

#Lo siguiente representa el archivo inicial
#elegir ruta
#choose.files()
#importar nuevo csv provisional
#presupuestos_suministro <- read_delim("C:\\Users\\G513\\Desktop\\GIS\\3\\fuentes\\53448_def.csv",
                                      #delim = ";", escape_double= FALSE, trim_ws= TRUE)
#view(presupuestos_suministro)

#Segunda prueba de archivo
#eligiendo ruta
choose.files()
#importando--
sum_presupuestos <- read_delim ("C:\\Users\\G513\\Desktop\\GIS\\3\\fuentes\\sum_presupuesto.csv",
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
view(sum_presupuestos)
#comprobando codificacion de los datos
sum_presupuestos_compr <- read.csv("C:\\Users\\G513\\Desktop\\GIS\\3\\fuentes\\sum_presupuesto.csv", fileEncoding = "UTF-8")
