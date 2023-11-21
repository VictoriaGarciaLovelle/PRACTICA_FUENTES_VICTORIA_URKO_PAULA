# Ejemplo basado en 
# https://bookdown.org/Maxine/r4ds/pdf.html
# https://www.brodrigues.co/blog/2018-06-10-scraping_pdfs/

install.packages("dplyr")
library(dplyr)
library(pdftools)

#extract data original
txt1 <- pdf_text("Datos_Agua.pdf")
txt1
str(txt1)

# first page text
pag <- cat(txt1[17])

install.packages("stringr")
library(stringr)

# split the text into a one-row text matrix
text_matrix <- txt1 %>%
  str_split("\n", simplify = TRUE)

text_matrix[17]
str(text_matrix[17])

