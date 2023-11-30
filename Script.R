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

tablaEsperanzaDeVida <- tablaEsperanzaDeVida %>%
  mutate(ComunidadAutonoma = toupper(ComunidadAutonoma))
tablaEsperanzaDeVida

## ------------- Cantidad de agua consumida ----------------------
archivoJsonCantidad <- fromJSON(file = "CantidadAgua.json")

cantidadAgua <- spread_all(archivoJsonCantidad)

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

tablaCantidadDeAgua <- tablaCantidadDeAgua %>%
  mutate(ComunidadAutonoma = toupper(ComunidadAutonoma))
tablaCantidadDeAgua

tablaCantidadDeAgua[4,2] <- c("BALEARS, ILLES")
tablaCantidadDeAgua[8,2] <- c("CASTILLA - LA MANCHA")
tablaCantidadDeAgua[14,2] <- c("MADRID, COMUNIDAD DE")
tablaCantidadDeAgua[15,2] <- c("MURCIA, REGIÓN DE")
tablaCantidadDeAgua[16,2] <- c("NAVARRA, COMUNIDAD FORAL DE")
tablaCantidadDeAgua[18,2] <- c("RIOJA, LA")
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
datos_obtenidos <- datos_divididos[-c(1:3)]
datosDeInteres <- datos_obtenidos[-c(length(datos_obtenidos))]
datosDeInteres[[4]] <- c("CASTILLA - LA MANCHA","28","39","43","24","7","12","0")
datosDeInteres[[5]] <- c("CASTILLA Y LEÓN","2","2","2","0","1","1","0")
datosDeInteres[[9]] <- c("MADRID, COMUNIDAD DE","6","6","7","0","2","5","0")
datosDeInteres[[10]] <- c("MURCIA, REGIÓN DE","3","3","3","0","0","3","0")
datosDeInteres[[11]] <- c("NAVARRA, COMUNIDAD FORAL DE","11","11","11","4","5","2","0")
datosDeInteres[[12]] <- c("RIOJA, LA","1","1","1","0","1","0","0")
datosDeInteres[[13]] <- c("COMUNITAT VALENCIANA","2","2","2","0","1","1","0")
datosDeInteres

tablaCalidadDeAgua<-data.frame()
for (i in datosDeInteres){
  tablaCalidadDeAgua<-rbind(tablaCalidadDeAgua,i)
}

tablaCalidadDeAgua

# Ajustar los nombres de las columnas
colnames(tablaCalidadDeAgua) <- c("Comunidad Autónoma", "Num de Municipios", "Zonas de baño", "Puntos de muestreo","Aguas 2", "Aguas 1", "Aguas 0", "Aguas SCF")
tablaCalidadDeAgua

colNumericas <- c("Num de Municipios", "Zonas de baño", "Puntos de muestreo", "Aguas 2", "Aguas 1", "Aguas 0", "Aguas SCF")
tablaCalidadDeAgua <- tablaCalidadDeAgua %>%
  mutate_at(vars(colNumericas), as.integer)

tablaCalidadDeAgua
str(tablaCalidadDeAgua)
## -------------------- Presupuestos del agua ----------------------------

#importando archivo
summodificado <- read_csv("summodificado.csv")

#Modificando el csv
sum_ <- select(.data = summodificado, "Comunidades y Ciudades Autónomas":Total) %>% 
            drop_na()   %>% 
  filter(`Grupos de usuarios e importe`=="Importe total de la inversión en los servicios de suministro" & periodo== "2020")

#  mutate(`Comunidades y Ciudades Autónomas`= str_replace(`Comunidades y Ciudades Autónomas`,"01 Andalucía" , "ANDALUCÍA")) %>% 
#    mutate(`Comunidades y Ciudades Autónomas`= str_replace(`Comunidades y Ciudades Autónomas`,"02 Aragón" , "ARAGÓN")) %>% 
#    mutate(`Comunidades y Ciudades Autónomas`= str_replace(`Comunidades y Ciudades Autónomas`,"03 Asturias, Principado de" , "ASTURIAS")) %>% 
#    mutate(`Comunidades y Ciudades Autónomas`= str_replace(`Comunidades y Ciudades Autónomas`,"04 Balears, Illes" , "BALEARES")) %>% 
#    mutate(`Comunidades y Ciudades Autónomas`= str_replace(`Comunidades y Ciudades Autónomas`,"05 Canarias" , "CANARIAS")) %>% 
#    mutate(`Comunidades y Ciudades Autónomas`= str_replace(`Comunidades y Ciudades Autónomas`,"06 Cantabria" , "CANTABRIA")) %>%
#    mutate(`Comunidades y Ciudades Autónomas`= str_replace(`Comunidades y Ciudades Autónomas`,"07 Castilla y León" , " CASTILLA-Y-LEÓN")) %>% 
#    mutate(`Comunidades y Ciudades Autónomas`= str_replace(`Comunidades y Ciudades Autónomas`,"08 Castilla - La Mancha" , "CASTILLA-LA MANCHA")) %>% 
#    mutate(`Comunidades y Ciudades Autónomas`= str_replace(`Comunidades y Ciudades Autónomas`,"09 Cataluña" , "CATALUÑA")) %>% 
#    mutate(`Comunidades y Ciudades Autónomas`= str_replace(`Comunidades y Ciudades Autónomas`,"10 Comunitat Valenciana" , "VALENCIA")) %>% 
#    mutate(`Comunidades y Ciudades Autónomas`= str_replace(`Comunidades y Ciudades Autónomas`,"11 Extremadura" , "EXTREMADURA")) %>% 
#    mutate(`Comunidades y Ciudades Autónomas`= str_replace(`Comunidades y Ciudades Autónomas`,"12 Galicia" , "GALICIA")) %>% 
#    mutate(`Comunidades y Ciudades Autónomas`= str_replace(`Comunidades y Ciudades Autónomas`,"13 Madrid, Comunidad de" , "MADIRD")) %>% 
#    mutate(`Comunidades y Ciudades Autónomas`= str_replace(`Comunidades y Ciudades Autónomas`,"14 Murcia, Región de" , "MURCIA")) %>% 
#    mutate(`Comunidades y Ciudades Autónomas`= str_replace(`Comunidades y Ciudades Autónomas`,"15 Navarra, Comunidad Foral de" , "NAVARRA")) %>% 
#    mutate(`Comunidades y Ciudades Autónomas`= str_replace(`Comunidades y Ciudades Autónomas`,"16 País Vasco" , "PAÍS VASCO")) %>% 
#    mutate(`Comunidades y Ciudades Autónomas`= str_replace(`Comunidades y Ciudades Autónomas`,"17 Rioja, La" , "RIOJA"))

sum_  



