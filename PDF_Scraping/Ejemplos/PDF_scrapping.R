# Instalar y cargar la librería pdftools
install.packages("pdftools")
library(pdftools)

# Ruta al archivo PDF
ruta_pdf <- pdf_text("Datos_Agua.pdf")

# Especificar la página del PDF que quieres procesar
pagina <- ruta_pdf[17]
pagina

# Procesar el texto para obtener los datos deseados
# Aquí debes aplicar algún método específico para extraer los datos que necesitas
# Puede ser mediante expresiones regulares, funciones de manipulación de texto, etc.

# Ejemplo: Dividir el texto por líneas y seleccionar la línea deseada
lineas <- strsplit(pagina, "\n")[[1]]
lineas
linea_deseada <- lineas[5:21]  # Ajusta el índice según tu necesidad

# Imprimir la línea deseada
linea_deseada[4]
