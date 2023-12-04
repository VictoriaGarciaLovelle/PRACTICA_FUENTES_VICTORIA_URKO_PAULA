# Importación de los datos ----
#install.packages("pdftools")
library(pdftools)
library(glue)
library(readr)
library(rjson)
library(tidyverse)
library(tidyjson)
library(stringr)
library(purrr) #Función lmap aplica una función a cada lista

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
  select(Nombre, Anyo, Valor) 

#Selecciono las columnas que me interesan
#seleccion<-select(.data = arrayData, Nombre, Anyo, Valor)

# Utilizar strsplit para dividir la cadena de texto
#separador <- "\\."
partes <- strsplit(arrayData$Nombre, "\\.")

comunidadesAutonomas<-c()
for (i in partes){
  comunidadesAutonomas<-c(comunidadesAutonomas,i[1])
}

#Cambio la columna nombre por la de las comunidades autónomas
arrayData$Nombre<-comunidadesAutonomas

#Para eliminar una columna
tablaComunidadesAñoValor<- as_tibble(arrayData)
attr(tablaComunidadesAñoValor, "JSON") <- NULL

# Cuento cuantas hay (para ver si coincide con las que hay en el excel)
#conteo_andalucia <- sum(seleccion$Nombre == "Andalucía")
#print(conteo_andalucia)

#Agrupo por años (varios años de 2016 para cada c.a) y calculo la media de la esperanza de vida
tablaEsperanzaDeVida <- tablaComunidadesAñoValor %>%
  filter(Anyo==2020)%>%
  group_by(Anyo, Nombre) %>%
  summarize(EsperanzaDeVida = mean(Valor, na.rm = TRUE))%>%
  rename(Año=Anyo,ComunidadAutonoma=Nombre)

tablaEsperanzaDeVidaFinal <- tablaEsperanzaDeVida %>%
  mutate(ComunidadAutonoma = toupper(ComunidadAutonoma))

colnames(tablaEsperanzaDeVidaFinal) <- c("Anio", "ComunidadAutonoma", "EsperanzaDeVida")
tablaEsperanzaDeVidaFinal

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

#Divido la cadena de texto nombre y solo cojo las comunidades autónomas
cadenas <- strsplit(arrayDataCantidad$Nombre, "\\,")

for (i in cadenas){
  comunidadesAutonomasCantidad<-c(comunidadesAutonomasCantidad,i[1])
}

#Cambio la columna nombre por la columna de las comunidades autónomas
arrayDataCantidad$Nombre<-comunidadesAutonomasCantidad

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

tablaCantidadDeAgua1 <- tablaCantidadDeAgua %>%
  mutate(ComunidadAutonoma = toupper(ComunidadAutonoma))

tablaCantidadDeAgua1[4,2] <- c("BALEARS, ILLES")
tablaCantidadDeAgua1[8,2] <- c("CASTILLA - LA MANCHA")
tablaCantidadDeAgua1[14,2] <- c("MADRID, COMUNIDAD DE")
tablaCantidadDeAgua1[15,2] <- c("MURCIA, REGIÓN DE")
tablaCantidadDeAgua1[16,2] <- c("NAVARRA, COMUNIDAD FORAL DE")
tablaCantidadDeAgua1[18,2] <- c("RIOJA, LA")

colnames(tablaCantidadDeAgua1) <- c("Anio", "ComunidadAutonoma", "Cantidad")

tablaCantidadDeAguaFinal <- tablaCantidadDeAgua %>%
  mutate_at(vars(Anio), as.integer)
tablaCantidadDeAguaFinal

## --------------------- Calidad del agua ----------------------------
# Ruta al archivo PDF
ruta_pdf <- pdf_text("report_Cap.3_part2._Libro_blanco_del_agua.pdf")

# Especificar la página del PDF que quieres procesar
pagina <- ruta_pdf[17]

lineas <- strsplit(pagina, "\n")[[1]]
linea_deseada <- lineas[5:21]  # Ajusta el índice según tu necesidad

# Dividir las líneas en columnas
datos_divididos <- strsplit(linea_deseada, "\\s+")

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

tablaCalidadDeAgua<-data.frame()
for (i in datosDeInteres){
  tablaCalidadDeAgua<-rbind(tablaCalidadDeAgua,i)
}

colnames(tablaCalidadDeAgua) <- c("ComunidadAutonoma", "NumdeMunicipios", "ZonasdeBaño","PuntosdeMuestreo","Aguas2", "Aguas1","Aguas0", "AguasSCF")

colNumericas <- c("ComunidadAutonoma", "NumdeMunicipios", "ZonasdeBaño","PuntosdeMuestreo","Aguas2", "Aguas1","Aguas0", "AguasSCF")
tablaCalidadDeAguaFinal <- tablaCalidadDeAgua %>%
  mutate_at(vars(colNumericas), as.integer)

tablaCalidadDeAgua
## -------------------- Presupuestos del agua ----------------------------

#importando archivo
summodificado <- read_csv("summodificado.csv")
colnames(summodificado) <- c("TotalNacional", "ComunidadAutonoma", "GruposDeUsuarioEImporte","Anio", "Presupuesto")

#Modificando el csv
tablaPresupuestos <- summodificado%>%
  filter(GruposDeUsuarioEImporte=="Importe total de la inversión en los servicios de suministro" & Anio== "2020") %>%
  mutate(ComunidadAutonoma = gsub("^\\d+\\s*", "", ComunidadAutonoma)) %>%
  mutate(ComunidadAutonoma = toupper(ComunidadAutonoma)) %>%
  select (.data = ., Anio, ComunidadAutonoma:Presupuesto) %>%
  drop_na()

tablaPresupuestos$Presupuesto <- gsub("\\.", "", tablaPresupuestos$Presupuesto)
tablaPresupuestos$Presupuesto <- as.integer(tablaPresupuestos$Presupuesto)
tablaPresupuestos

#----------------------------Joins---------------------------------------------------
EsperanzayCantidad<- tablaEsperanzaDeVida%>%
  left_join(x=., y=tablaCantidadDeAgua, by=c("Anio","ComunidadAutonoma"))%>%
  group_by(ComunidadAutonoma) %>%
  drop_na()

#--grafico Esperanza y cantidad---
library(tidyr)
ggplot(data=EsperanzayCantidad, aes(x=Cantidad, y=EsperanzaDeVida))+
  geom_point(aes(color=ComunidadAutonoma))+
  geom_smooth()+
  labs(title="Cantidad de agua junto esperanza de vida por Comunidades Autonomas",
       x="Cantidad de agua ",
       y="Esperanza de vida")+
  theme_minimal()

#--Cantidad y presupuesto--
CantidadyPresupuesto<- tablaCantidadDeAgua%>% 
  left_join(x=., y=tablaPresupuestos, by=c("Anio","ComunidadAutonoma")) %>% 
  select(-GruposDeUsuarioEImporte) %>%
  arrange(desc(Presupuesto)) %>%
  drop_na()

#.--Grafico Cantidad y presupuesto--

ggplot(data=CantidadyPresupuesto1, aes(x= Presupuesto, y= Cantidad, fill=ComunidadAutonoma))+
  geom_bar(stat= "identity")+
  labs(title="Cantidad de agua junto presupuestos por Comunidades Autonomas",
       x="Presupuestos",
       y="Cantidad de agua")+
  geom_bar(stat= "identity", width = 5000)

#--
EsperanzayCalidad<- tablaEsperanzaDeVida%>%
  left_join(x=., y=tablaCalidadDeAgua, by=c("ComunidadAutonoma"))%>%
  group_by(ComunidadAutonoma) %>%
  drop_na()

EsperanzayCalidadFinal <- pivot_longer(data = EsperanzayCalidad, names_to = "CalidadAgua", values_to = "ValoresCalidadAgua", cols = c(Aguas2,Aguas1,Aguas0,AguasSCF))
EsperanzayCalidadFinal

# como la variable drv tiene solo 3 niveles, podemos dividir el gráfico de acorde a ellas
graficoEsperanzaCalidad <- ggplot(data = EsperanzayCalidad1, aes(x = ValoresCalidadAgua, y = EsperanzaDeVida)) +
  geom_point(aes(colour = ComunidadAutonoma)) +
  facet_wrap(facets = vars(CalidadAgua), nrow = 1)+
  labs(title="Calidad de agua junto Esperanza de Vida por Comunidades Autonomas",
       x="Calidad",
       y="Esperanza de vida")
graficoEsperanzaCalidad

#-----
tablaFinal<- EsperanzayCantidad %>% 
  left_join(x=., y=CantidadyPresupuesto, by=c("Cantidad","ComunidadAutonoma","Anio")) %>%
  left_join(x=., y=EsperanzayCalidad1, by=c("ComunidadAutonoma")) %>% 
  select(-"NumdeMunicipios",- "ZonasdeBaño",-"PuntosdeMuestreo") %>% 
  drop_na()

tablaFinal

ggplot(data=tablaFinal, aes(x=Cantidad, y=EsperanzaDeVida.x))+
  geom_point(aes(colour = CalidadAgua))+
  geom_smooth()+
  labs(title="Presupuesto junto esperanza de vida por Comunidades Autonomas",
       x="Cantidad",
       y="EsperanzaDeVida.x")+
  theme_minimal()

# Mismos valores CalidadAgua
ggplot(data=tablaFinal, aes(x= Cantidad, y= EsperanzaDeVida.x, fill=CalidadAgua))+
  geom_bar(stat= "identity")+
  labs(title="Presupuesto junto esperanza de vida por Comunidades Autonomas",
       x="Cantidad",
       y="EsperanzaDeVida.x")+
  theme_minimal()

ggplot(data=tablaFinal, aes(x=Presupuesto, y=EsperanzaDeVida.x, color=ComunidadAutonoma))+
  geom_point()+
  labs(title="Presupuesto junto esperanza de vida por Comunidades Autonomas",
       x="Presupuesto",
       y="Esperanza de vida")+
  theme_minimal()

