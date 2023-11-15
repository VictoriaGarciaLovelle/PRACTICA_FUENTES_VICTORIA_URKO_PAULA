# Ejemplo basado en 
# https://bookdown.org/Maxine/r4ds/pdf.html
# https://www.brodrigues.co/blog/2018-06-10-scraping_pdfs/

library(pdftools)
# Archivo ejemplo
download.file("http://arxiv.org/pdf/1403.2805.pdf", "PDF_Scraping/Ejemplos/1403.2805.pdf", mode = "wb")

#archivo agua
download.file("https://www.chj.es/es-es/medioambiente/planificacionhidrologica/Documents/Plan%20de%20Recuperaci%C3%B3n%20del%20J%C3%BAcar/Cap.3_part2._Libro_blanco_del_agua.pdf", "PDF_Scraping/Ejemplos/Datos_Agua.pdf", mode = "wb")

#extract data original
txt <- pdf_text("PDF_Scraping/Ejemplos/1403.2805.pdf")
txt
str(txt)

#extract data archivo agua
txt1 <- pdf_text("PDF_Scraping/Ejemplos/Datos_Agua.pdf")
str(txt1)

# first page text
cat(txt[1])
cat(txt1[1])

# second page text
cat(txt[2])
cat(txt1[2])

#  # split the text into a one raw text matrix
text_matrix <- txt1 %>% 
  str_split("\\n", simplify = TRUE) 

View(text_matrix)

text_matrix[1,7]
