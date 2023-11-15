# Nuestro directorio
getwd()
list.files()

# En nuestro directorio debe haber una carpeta llamada INPUT que contenta a su vez otra carpeta Informes donde 
#se almacenen los ficheros.pdf con los que vamos a trabajar.
ruta <- paste(getwd(), "/INPUT/Informes", sep = "")
list.files(ruta)


library(tools) #Sin esto no funciona file_ext
library(pdftools) #Función LeerDocumentos
library(readxl) #Lectura de ficheros Excel
library(stringr)


LeerFicherosPDF <- function(ruta) {
  ficheros <- list()
  contenido <- list.files(ruta, recursive = TRUE)
  #print(contenido)
  for (fichero in contenido) {
    if (file.exists(file.path(ruta, fichero)) && tolower(tools::file_ext(fichero)) == "pdf") {
      ficheros <- append(ficheros, fichero)
    }
  }
  return(ficheros)
}
ficheros <- LeerFicherosPDF(ruta)
#Ficheros me almacena los ficheros acabados en .pdf de la ruta en la que estoy
print(ficheros)



LeerDocumento <- function(ruta){
  archivos <- list.files(ruta, full.names = TRUE)
  text <- character(length(archivos))
  for (i in seq_along(archivos)) {
    pdf_texto <- pdf_text(archivos[i])
    text[i] <- paste(pdf_texto, collapse = "\n")
  } 
  return(text)
}
#Devuelve el texto de todos y cada uno de los archivos pdf de la ruta.
text


BuscarValor<- function(textoABuscar, text){
  Encontrados <- vector()
  valores <- ifelse(grepl(textoABuscar, text), 1, 0)
  valores <- which(valores == 1)
  posiciones <- nchar(textoABuscar)
  for (i in which(valores == 1)){
    Encontrados <- append(Encontrados, substr(lines[i], start = (nchar(lines[i]) - nchar(textoBuscar) - posiciones + 2), stop =  nchar(lines[i])))
  }
  return (Encontrados)
}
Encontrados


# FALTA LA TERCERA FUNCIÓN!!!





## LECTURA DE FICHEROS:

Diagnostico <- read_excel("INPUT/Datos/Diagnostico.xlsx")
View(Diagnostico)

Genes <- read_excel("INPUT/Datos/Genes.xlsx")
View(Genes)

# Creación de diccionarios partiendo de los ficheros Excel. 
# Primero se definene los valores y luego las claves. 
diagnosticos_dic <- setNames(Diagnostico$`NÚMERO DIAGNÓSTICO`, Diagnostico$DIAGNÓSTICO)
diagnosticos_dic
#Names() nos permite recorrer las claves del diccionario y unname() obtener el valor. 
#Cat() imprime el formato (clave:valor)
for (diag in names(diagnosticos_dic)) {
  valor <- unname(diagnosticos_dic[diag])
  cat(diag, ":", valor, "\n")
}


mutaciones_dic <- setNames(Genes$`Número gen`, Genes$GEN)
mutaciones_dic
#Nos interesa tener una variable que almacene solo los nombres de los genes. 
mutaciones <- unique(Genes$GEN)
mutaciones
for (gen in names(mutaciones_dic)) {
  valor <- unname(mutaciones_dic[gen])
  cat(gen, ":", valor, "\n")
}




BuscarValor <- function(textoBuscar, lines) {
  Encontrados <- vector()
  
  valores <- ifelse(grepl(textoBuscar, lines), 1, 0)
  valores <- which(valores == 1)
  
  posiciones <- nchar(textoBuscar)
  
  for (i in valores) {
    Encontrados <- append(Encontrados, substr(lines[i], start = nchar(lines[i]) - nchar(textoBuscar) - posiciones + 2, stop =  nchar(lines[i])))
  }
  
  return (Encontrados)
}

fecha_Data <- vector()
NHC_Data <- vector()
Nbiopsia_Data <- vector()
texto_Data <- vector()


for (ficheroPDF in ficheros) {
  lines <- LeerDocumento(file.path(Ruta, ficheroPDF))
  
  NHC_Data <- append(NHC_Data, BuscarValor("NHC:", lines))
  Nbiopsia_Data <- append(Nbiopsia_Data, BuscarValor("biopsia:", lines))
  fecha_Data <- append(fecha_Data, BuscarValor("Fecha:", lines))
  texto_Data <- append(texto_Data, BuscarValor("de la muestra:", lines))
}
print(NHC_Data)




BuscarValor <- function(textoBuscar, lines) {
  Encontrados <- c()
  
  for (i in seq_along(lines)) {
    if (grepl(textoBuscar, lines[i])) {
      Encontrados <- c(Encontrados, trimws(sub(paste0(".*", textoBuscar, "\\s*"), "", lines[i])))
    }
  }
  
  return(Encontrados)
}
resultado2 <- BuscarValor("Fecha:", text)
print(resultado2)


LeerValor <- function(ruta, palabra) {
  archivos <- list.files(ruta, full.names = TRUE)
  valores <- numeric(length(archivos))
  for (i in seq_along(archivos)) {
    texto <- pdf_text(archivos[i])
    patron <- paste0(palabra, "\\s*([[:digit:].,]+)")
    resultado <- regmatches(texto, regexpr(patron, texto, perl = TRUE))
    if (length(resultado) > 0) {
      valor <- as.numeric(gsub(",", ".", sub(paste0(palabra, "\\s*"), "", resultado)))
      valores[i] <- valor
    } else {
      valores[i] <- NA
    }
  }
  return(valores)
}
res <- LeerValor(ruta, "Fecha: ")
res

a <- LeerValor(ruta, "Nº biopsia: ")
a

b <- LeerValor(ruta, "NHC: ")
b

c <- LeerValor(ruta, "de la muestra: ")
c
