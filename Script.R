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

#Viendo los arrays en el tibble 1, entro en el array Data
arrays<-esperanzaVida %>%
  enter_object(Data) %>% 
  gather_array %>% 
  spread_all %>% 
  select(-document.id, -array.index) 

arrays


# Así puedo acceder a la comunidad autónomaselect(.data = sum_presupuestos, !(ends_with("Hogares"))& !(ends_with("económicos"))& !(ends_with("municipales")) &

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
#encontrando ruta
choose.files()
sum_presupuestos <- read_delim ("C:\\Users\\G513\\Desktop\\GIS\\3\\fuentes\\presupuesto_de_suministro_de_agua.csv",
                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)
view(sum_presupuestos)     

#intento 3


x<- select(.data = sum_presupuestos, ends_with("inversión en los servicios de suministro"))
print(x)











#Segunda prueba de archivo
#eligiendo ruta
#choose.files()
#importando--
#sum_presupuestos <- read_delim ("C:\\Users\\G513\\Desktop\\GIS\\3\\fuentes\\sum_presupuesto.csv",
#                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

#viendo si se trata de un error de codificacion
#codificacion_de_csv <- guess_encoding("C:\\Users\\G513\\Desktop\\GIS\\3\\fuentes\\sum_presupuesto.csv")
#print(codificacion_de_csv)
# solucion del error
#sum_presupuestos <- read.csv("C:\\Users\\G513\\Desktop\\GIS\\3\\fuentes\\sum_presupuesto.csv", fileEncoding = "ISO-8859-1")
#view(sum_presupuestos)

