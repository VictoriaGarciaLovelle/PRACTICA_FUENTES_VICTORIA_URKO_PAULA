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
library(purrr) #Función lmap aplica una función a cada lista
library(dplyr)

##----------------- Esperanza de vida -------------------
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
tablaEsperanzaDeVida <- tablaComunidadesAñoValor %>%
  filter(Anyo==2020)%>%
  group_by(Anyo, Nombre) %>%
  summarize(EsperanzaDeVida = mean(Valor, na.rm = TRUE))%>%
  rename(Año=Anyo,ComunidadAutonoma=Nombre)
tablaEsperanzaDeVida


## ------------- Cantidad de agua consumida ----------------------
archivoJsonCantidad <- fromJSON(file = "CantidadAgua.json")

cantidadAgua <- spread_all(archivoJsonCantidad)
View(cantidadAgua)

#Veo los tipos de atributos
cantidadAgua%>%
  gather_object%>%
  json_types%>%
  count(name,type)

#Entro en el array Data
arrayDataCantidad<-cantidadAgua%>%
  enter_object(Data)%>%
  gather_array%>%
  spread_all%>%
  select(-document.id,-array.index)
  
arrayDataCantidad

#Divido la cadena de texto nombre y solo cojo las comunidades autónomas
separador<- "\\,"

cadenas <- strsplit(arrayDataCantidad$Nombre, separador)
cadenas
comunidadesAutonomasCantidad<-c()
for (i in cadenas){
  comunidadesAutonomasCantidad<-c(comunidadesAutonomasCantidad,i[1])
}
comunidadesAutonomasCantidad

#Cambio la columna nombre por la columna de las comunidades autónomas
arrayDataCantidad$Nombre<-comunidadesAutonomasCantidad
arrayDataCantidad

#Tengo que eliminar de la tabla las comunidades autónomas donde ponga españa
tabla <- arrayDataCantidad %>%
  filter(!(Nombre == "España")) %>%
  select(Nombre, NombrePeriodo, Valor)
  
#Agrupo por comunidades autónomas y años
tablaCantidadDeAgua <- tabla %>%
  filter(NombrePeriodo  ==2020)%>%
  group_by(NombrePeriodo  , Nombre) %>%
  summarize(Cantidad = mean(Valor, na.rm = TRUE))%>%
  rename(Año=NombrePeriodo,ComunidadAutonoma=Nombre)
tablaCantidadDeAgua


## --------------------- Calidad del agua ----------------------------
# Ruta al archivo PDF
ruta_pdf <- pdf_text("report_Cap.3_part2._Libro_blanco_del_agua.pdf")

# Especificar la página del PDF que quieres procesar
pagina <- ruta_pdf[17]
pagina

lineas <- strsplit(pagina, "\n")[[1]]
lineas
linea_deseada <- lineas[5:21]  # Ajusta el índice según tu necesidad

# Imprimir la línea deseada
linea_deseada

# Dividir las líneas en columnas
datos_divididos <- strsplit(linea_deseada, "\\s+")
datos_divididos

# Eliminación de las primeras 3 dimensiones pertenecientes a los nombres de las columnas
datos_divididos <- datos_divididos[-c(1:3)]
datos_divididos[[4]]<- c("CASTILLA-LA-MANCHA", "28", "39", "43", "24","7", "12", "0" )
datos_divididos[[5]]<- c("CASTILLA-Y-LEÓN", "2", "2", "2", "0", "1", "1", "0"   )
datos_divididos

tablaCalidadDeAgua<-data.frame()
for (i in datos_divididos){
  tablaCalidadDeAgua<-rbind(tablaCalidadDeAgua,i)
}
tablaCalidadDeAgua

# Ajustar los nombres de las columnas
colnames(tablaCalidadDeAgua) <- c("Comunidad Autónoma", "Num de Municipios", "Zonas de baño", "Puntos de muestreo","Aguas 2", "Aguas 1", "Aguas 0", "Aguas SCF")
tablaCalidadDeAgua

## -------------------- Presupuestos del agua ----------------------------

#encontrando ruta . 
choose.files()
#importando archivo
#summodificado <- read_delim ("summodificado.csv",
                      # delim = ";", escape_double = FALSE, trim_ws = TRUE)
summodificado <- read_csv("summodificado.csv")
view(summodificado)
#print (summodificado)
#modificando la tabla 
#summodificado<- filter(.data = summodificado,"Grupos de usuarios e importe" %in% c("Importe total de la inversión en los servicios de suministro")) %>% 
  #select("Comunidades y Ciudades Autónomas":"Total") %>% 
 #           drop_na()
view(sum_)

sum_ <- select(.data = summodificado, "Comunidades y Ciudades Autónomas":Total) %>% 
            drop_na()  








