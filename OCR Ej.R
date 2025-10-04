
library(tesseract)

#LSTM Long short term memory (Red neuronal recurrente)

eng <- tesseract("eng")
texto <- tesseract::ocr("http://jeroen.github.io/images/testocr.png", engine = eng)
cat(texto)

result <- tesseract::ocr_data("http://jeroen.github.io/images/testocr.png", engine = eng)
tesseract_info()

#Instalar Holandes
tesseract_download("nld")
dutch <- tesseract("nld")

text_dutch <- ocr("https://jeroen.github.io/images/utrecht2.png", engine = dutch)
cat(text_dutch)

#Para revisar calidad de la imagen para mejorar la imagen
library(magick)

img_eng <- image_read("https://jeroen.github.io/images/bowers.jpg")
analisis <- img_eng %>%
  image_resize("2000x") %>%
  image_convert(type = "Grayscale") %>%
  image_trim(fuzz = 40) %>%
  image_write(format = "png", density = "300x300") %>%
  tesseract::ocr()

cat(analisis)


# Imagenes en PDF

texto_pdf <- pdftools::pdf_convert("https://jeroen.github.io/images/ocrscan.pdf", dpi = 700)
pdf <- tesseract::ocr(texto_pdf)
cat(pdf)

#Ejercicio de factura

factura <- tesseract(options = list(tessedit_char_whitelist = "$.0123456789")) 
cat(ocr("https://jeroen.github.io/images/receipt.png", engine = factura))

factura2 <- tesseract(options = list(tessedit_char_whitelist = ".0123456789")) 
cat(ocr("https://jeroen.github.io/images/receipt.png", engine = factura2))

factura3 <- tesseract(options = list(tessedit_char_whitelist = "USD.0123456789")) 
cat(ocr("https://jeroen.github.io/images/receipt.png", engine = factura3))

#Convertir imagenes por extensiones de linea
library(dplyr)
library(magick)
library(tesseract)
library(tidyverse)

tabla_datos <- "https://i.stack.imgur.com/V9lWV.png" %>%
  image_read() %>%
  image_resize("2000x") %>%
  image_convert(type = "Grayscale") %>%
  image_trim (fuzz = 40) %>%
  image_write(format = "png", density = "300x300") %>%
  tesseract::ocr() %>%
  strsplit("\n") %>%
  getElement(1) %>%
  '[' (-1) %>%
  {sub('Time 2','Time_2',.)} %>%
  {read.table(text = .)} %>%
  setNames(c("Time_factor", "Tree #", "Species", "Fragment", "Linear extension(mm)", "Colour"))

library(DT)  
datatable(tabla_datos)
tabla_datos$Time_factor <- as.factor(tabla_datos$Time_factor)

library(ggplot2)
ggplot(tabla_datos,
       aes(x = Time_factor, y = `Linear extension(mm)`, fill = Colour)) +
  geom_col() +
  scale_fill_manual(values = c("blue","brown","yellow")) +
  theme_light() +
  labs(title = "Linear extension")
