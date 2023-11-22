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

# INICIALIZACIÓN DE LA PRUEBA 1 ----------------------------------------------------------------------------
# Conjunto de datos de la tabla
datos <- c(
  "                 Num. de",
  "Comunidad Autónoma                       Zonas de        Puntos de         Aguas            Aguas           Aguas     Aguas",
  "                Municipios                baño           muestreo           “2”              “1”             “0”       SCF",
  "ANDALUCÍA           58                      63              70               3                36             27          4",
  "ARAGÓN              11                      11              12               3                7               1          1",
  "ASTURIAS             1                       1               1               0                0               1         0",
  "CASTILLA-LA MANCHA 28                       39              43              24                 7             12          0",
  "CASTILLA Y LEÓN      2                       2               2               0                1               1         0",
  "CATALUÑA             9                      10              11               3                8               0         0",
  "EXTREMADURA         17                      17              17               0                0               0         17",
  "GALICIA             53                      54              68              10                45             13          0",
  "MADRID               6                       6               7               0                2               5         0",
  "MURCIA               3                       3               3               0                0               3         0",
  "NAVARRA             11                      11              11               4                 5              2          0",
  "RIOJA                1                      1                1               0                1               0         0",
  "VALENCIA             2                       2               2               0                1               1         0",
  "TOTAL              202                     220             248              48               112             66         22"
)

# Eliminar las comillas y espacios en blanco adicionales
datos_limpio <- gsub('“|”', '', datos)
datos_limpio <- trimws(datos_limpio)

# Dividir las líneas en columnas
datos_divididos <- strsplit(datos_limpio, "\\s+")

# Transponer la matriz para tener columnas como variables
datos_transpuesto <- t(datos_divididos)

# Crear el marco de datos
df <- as.data.frame(datos_transpuesto, stringsAsFactors = FALSE)

# Ajustar los nombres de las columnas
colnames(df) <- df[1, ]

# Eliminar la primera fila
df <- df[-1, ]

# Convertir las columnas numéricas a tipo numérico
cols_numericas <- colnames(df)[2:ncol(df)]
df[cols_numericas] <- lapply(df[cols_numericas], as.numeric)

# Imprimir el marco de datos resultante
df
# FINALIZACIÓN DE LA PRUEBA 1 --------------------------------------------------------------------------------
# INICIALIZACIÓN DE LA PRUEBA 2 ------------------------------------------------------------------------------
# Datos proporcionados
datos <- c(
  "                 Num. de",
  "Comunidad Autónoma                       Zonas de        Puntos de         Aguas            Aguas           Aguas     Aguas",
  "                Municipios                baño           muestreo           “2”              “1”             “0”       SCF",
  "ANDALUCÍA           58                      63              70               3                36             27          4",
  "ARAGÓN              11                      11              12               3                7               1          1",
  "ASTURIAS             1                       1               1               0                0               1         0",
  "CASTILLA-LA MANCHA 28                       39              43              24                 7             12          0",
  "CASTILLA Y LEÓN      2                       2               2               0                1               1         0",
  "CATALUÑA             9                      10              11               3                8               0         0",
  "EXTREMADURA         17                      17              17               0                0               0         17",
  "GALICIA             53                      54              68              10                45             13          0",
  "MADRID               6                       6               7               0                2               5         0",
  "MURCIA               3                       3               3               0                0               3         0",
  "NAVARRA             11                      11              11               4                 5              2          0",
  "RIOJA                1                      1                1               0                1               0         0",
  "VALENCIA             2                       2               2               0                1               1         0",
  "TOTAL              202                     220             248              48               112             66         22"
)

# Eliminar las comillas y espacios en blanco adicionales
datos_limpio <- gsub('“|”', '', datos)
datos_limpio <- trimws(datos_limpio)

# Dividir las líneas en columnas
datos_divididos <- strsplit(datos_limpio, "\\s+")

# Crear el marco de datos
df <- as.data.frame(do.call(rbind, datos_divididos), stringsAsFactors = FALSE)

# FINALIZACIÓN DE LA PRUEBA 2 --------------------------------------------------------------------------------
# Proceso de prueba
linea_deseada[4]

# Los datos proporcionados
table(linea_deseada)

## -------------------- Presupuestos del agua ----------------------------

#encontrando ruta . 
choose.files()
#importando archivo
sum__presupuestos <- read_delim ("summodificado.csv",
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
view(sum__presupuestos)
print (sum__presupuestos)
#modificando la tabla 
sum_<- filter(.data = sum__presupuestos,"Grupos de usuarios e importe2" %in% c("Importe total de la inversión en los servicios de suministro")) %>% 
  select("Comunidades y Ciudades Autónomas":Total) %>% 
            drop_na()








