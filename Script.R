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

#Viendo los arrays en el tibble 1, entro en el array Data, de éste puedo obtener el valor de la esperanza de vida, los años y el nombre de las comunidades Autónomas
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

#Cambio la columna nombre por la de las comunidades autónomas
seleccion$Nombre<-comunidadesAutonomas

#Para eliminar una columna
tablaComunidadesAñoValor<- as_tibble(seleccion)
attr(tablaComunidadesAñoValor, "JSON") <- NULL
tablaComunidadesAñoValor

# Cuento cuantas hay (para ver si coincide con las que hay en el excel)
#conteo_andalucia <- sum(seleccion$Nombre == "Andalucía")
#print(conteo_andalucia)

#Agrupo por años (varios años de 2016 para cada c.a) y calculo la media de la esperanza de vida
tabla_agrupada <- tablaComunidadesAñoValor %>%
  group_by(Anyo, Nombre) %>%
  summarize(EsperanzaDeVida = mean(Valor, na.rm = TRUE))
tabla_agrupada
#print(tabla_agrupada, n = nrow(tabla_agrupada))

tablaFinal<-rename(.data = tabla_agrupada, Año = Anyo, ComunidadAutonoma = Nombre)
tablaFinal

#---------------------------------------------------------------------------
## Cantidad de agua consumida ----
cantidadDeAgua <- fromJSON(file = "CantidadAgua.json")

cantidadAgua <- spread_all(cantidadDeAgua)
View(cantidadAgua)


#---------------------------------------------------------------------------
## Calidad del agua ----


calidadDelAgua <- map_df(raw_text, clean_table)
view(calidadDelAgua)
#----Presupuestos del agua-------------------------------------------------

#encontrando ruta
choose.files()
#importando archivo
sum_presupuestos <- read_delim ("C:\\Users\\G513\\Desktop\\GIS\\3\\fuentes\\presupuesto_de_suministro_de_agua.csv",
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
#modificando la tabla 
sum_<- filter(sum_presupuestos,`Grupos de usuarios e importe` %in% c("Importe total de la inversión en los servicios de suministro")) %>% 
  select(`Comunidades y Ciudades Autónomas`:Total) %>% 
            drop_na()








