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
archivoJson <- fromJSON(file = "EsperanzaVida.json")

esperanzaVida <- spread_all(archivoJson)

# Para cada atributo me dice de qué tipo se trata
tibble1<-esperanzaVida %>% 
  gather_object %>%  
  json_types %>% 
  count(name, type)

#Quiero sacar los años, la comunidad autónoma, el valor (esperanza de vida)

#Viendo los arrays en el tibble 1, entro en el array Data, de éste puedo obtener el valor de la esperanza de vida
arrayData<-esperanzaVida %>%
  enter_object(Data) %>% 
  gather_array %>% 
  spread_all %>% 
  select(-document.id, -array.index) 
arrayData

#Selecciono las columnas que me interesan
seleccion<-select(.data = arrayData, Nombre, Anyo, Valor)

# Utilizar strsplit para dividir la cadena de texto
separador <- "\\."
partes <- strsplit(seleccion$Nombre, separador)

comunidadesAutonomas<-c()
for (i in partes){
  comunidadesAutonomas<-c(comunidadesAutonomas,i[1])
}
comunidadesAutonomas
seleccion$Nombre<-comunidadesAutonomas

#Tabla buena
tablaComunidadesAñoValor<- as_tibble(seleccion)
attr(tablaComunidadesAñoValor, "JSON") <- NULL
tablaComunidadesAñoValor

# Cuento cuantas hay (para ver si coincide con las que hay en el excel)
#conteo_andalucia <- sum(seleccion$Nombre == "Andalucía")
#print(conteo_andalucia)



#---------------------------------------------------------------------------
## Cantidad de agua consumida ----
cantidadDeAgua <- fromJSON(file = "CantidadAgua.json")

cantidadAgua <- spread_all(cantidadDeAgua)
View(cantidadAgua)


#---------------------------------------------------------------------------
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

