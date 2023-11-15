library(dplyr) #verbos que resuleven desafios de manipulaci?n mde datos m?s ocmunes (filter, mutate, select...)
library(magrittr)
library(tidyverse)
library(pdftools)
#library(stringr) viene incluido en tidyverse (permite trabajo f?cil con cadenas)
library(lubridate) #para fechas y horas

#Determinamos la ruta donde se encuentra nuestro documento.
txt <- pdf_text("C://Users/....Documentos/R/Sample_3_v88.pdf")
?pdf_text

str(txt)
txt

#Separamos las distintas hojas que puede llegar a tener, en este caso 3.
cat(txt[1])
cat(txt[2])
cat(txt[3])

#Separamos el texto de cada una de las l?neas que lo forman
raw_text <- pdftools::pdf_text("C://Users/...Documentos/R/Sample_3_v88.pdf") %>%   
  str_split("\n") %>%   
  #unlist nos permite extraer vectores para que queden bien separados
  unlist()
raw_text
-----------------------------------------------------------------------------------------------------------------------------
#Elegimos las filas que nos interesan
#N?mero chip (carpeta)
nchip <- raw_text[1]
chip <- raw_text[0]
chip
#N?mero paciente (carpeta)
paciente <- raw_text[2] #inventado
paciente
#Fecha informe
fecha <- unlist(str_split(raw_text[7], "\\s{2,}"))[3]
fecha
#NHC
NHC <- unlist(str_split(raw_text[7], "\\s{2,}"))[1]
NHC
#N?mero biopsia
Nbiopsia <- unlist(str_split(raw_text[7], "\\s{2,}"))[2]
Nbiopsia
#Biopsia s?lida
biopsia <- raw_text[3] #inventado
biopsia
#Texto diagn?stico
Texto <- raw_text[9]
Texto
#Mutaciones
raw_text[145]
raw_text[149]
raw_text[150]
mutaciones <- raw_text[c(145, 149, 150)]
mutaciones
mutacion <- unlist(str_split(raw_text[34], "\\s{2,}"))[1]
mutacion
#Frecuencia alelica %
raw_text[24]
frecuencia <- unlist(str_split(raw_text[34], "\\s{2,}"))[5]
frecuencia
#N?mero mutaci?n
nm <- unlist(str_split(raw_text[34], "\\s{2,}"))[3] #inventado
nm
#Tipo mutaci?n espec?fica (Extra)
espe <- unlist(str_split(raw_text[34], "\\s{2,}"))[2] 
espe
#F?rmaco aprobado
farmaco <-raw_text[14] #inventado
farmaco
# Ensayo cl?nico
ensayo <- raw_text[c(99:115)]
ensayo
#Benigno - maligno (extra)
bm <- unlist(str_split(raw_text[34], "\\s{2,}"))[9]
bm
#N?mero diagn?stico
Ndiagnostico <- raw_text[34]
Ndiagnostico

#CAMBIO UN FORMATO A FECHA (instalamos library(lubridate))
prueba_fecha <- unlist(str_split(raw_text[7], "\\s{2,}"))[3]
prueba_fecha
#fecha_final <- as.character(prueba_fecha, format="%d/%m/%Y")
#fecha_final
y<- dmy(prueba_fecha)
y
#str(y)
#mdy(y)
#ymd(prueba_fecha)

str(prueba_fecha)
str(fecha_final)
class(dmy(m))
fecha_final <- as.Date(as.character(prueba_fecha, format="%d/%m/%Y"))

#?NICA TABLA CON TODA LA INFORMACI?N
#Creo la tabla indicando inicio y fin de las filas usadas.
table_start <- stringr::str_which(raw_text, "Datos paciente: ")
table_end <- stringr::str_which(raw_text, "ERBB2, PDGFRA, PPARG")

table_trimmed <- raw_text[table_start:table_end] %>% 
  str_trim()
table_trimmed

#Como vemos que los espacios en blanco tambi?n se almacenan, los quitamos
squished_table <- str_replace_all(table_trimmed, "\\s{2,}", "|") %>% 
  str_remove_all(",")
squished_table
#No elimina los espacios que hay entre una fila y otra, sino que elimina los espacios dentro de una misma fila 
#ELIMINA EL ESPACIO ENTRE COLUMNAS EN UNA FILA
#Cambia los espacios de m?s de dos espacios (  ) por |

#Ahora que tenemos un vector de cadenas, podemos crear un tibble (o dataframe)
raw_df <- enframe(squished_table)
raw_df


#Separamos los valores en distintas columnas
new_df <- raw_df %>% 
  separate(value, 
           into = c("NHC", "Fecha", "Chip", "% frecuencia al?lica"), 
           sep = "\\|")
new_df


str(raw_text[7])
raw_text[7][1]
str_split(raw_text[7], "\\|")
unlist(str_split(raw_text[7], "\\s{2,}"))[3]
unlist(str_split(raw_text[7], "\\s{2,}"))[1]
?str_split

raw_text[c(149, 150, 145)]
---------------------------------------------------------------------------------------------------------------------------

#DISTINTAS TABLAS CON DISTINTAS MENOS INFORMACI?N
squished_table <- str_replace_all(table_trimmed, "\\s{2,}", "|") %>% 
  str_remove_all(",")
squished_table
squised_table2 <- str_replace_all(squished_table, ": ", "|")%>% 
  str_remove_all(",")
squised_table2

#Fecha informe
#m <- unlist(str_split(squised_table2[1], "\\|"))[5]
#m 
#class(m)
#class(dmy(m))


#CREAMOS LOS DISTINTOS DATAFRAMES PARA LAS TABLAS 
tabla1pacientes <- data.frame(
  "N? chip" = chip,
  "N? paciente" = paciente,
  "NHC" = NHC,
  "N? biopsia" = Nbiopsia,
  "Fecha informe" = y
)
tabla1pacientes

tabla2Biopsia <- data.frame(
  "Biopsia s?lida" = biopsia,
  "N? biopsia" = Nbiopsia,
  "Diagn?stico" = Texto,
  "Mutaciones detectadas" = mutacion
)
tabla2Biopsia

tabla3Mutaciones <- data.frame(
  "Mutaciones detectadas" = mutacion,
  "Tipo mutaci?n espec?fico" = espe,
  "Porcentaje frecuencia al?lica" = frecuencia,
  "Benigna/patog?nica" = bm,
  "Ensayos cl?nicos" = ensayo,
  "F?rmaco aprobado" = farmaco
)
tabla3Mutaciones

tabla4SubtipoMutacion <- data.frame(
  "Mutaciones detectadas" = mutacion,
  "Tipo mutaci?n espef?fica" = espe, 
  "N? mutaci?n" = nm
)
tabla4SubtipoMutacion

tabla5Diagnostico <- data.frame(
  "Diagn?stico" = Texto,
  "N? diagn?stico" = Ndiagnostico
)
tabla5Diagnostico

nombres = "a, b, c, d, e, f, g, h"
tabla6EspecificacionMutaciones <- data.frame(
  "Nombre mutaciones" = nombres,
  "N? mutacion" = nm
)
tabla6EspecificacionMutaciones
