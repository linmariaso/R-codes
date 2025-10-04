library(tesseract)
library(magick)

#Lectura y visualizacion de la imagen
img <- image_read("https://media.geeksforgeeks.org/wp-content/uploads/20190328185307/gfg28.png")
print(img)

#Agregar metodo OCR
text <- ocr(img)
print(text)

#Extraer informacion de la imagen
pdf <- pdftools::pdf_convert("https://www.africau.edu/images/default/sample.pdf", 
                             dpi = 600)
pdf_text <- ocr(pdf)
cat(pdf_text)

#Localizador de texto
#OCR DATA

install.packages(c("png", "tesseract", "magick", "boundingbox", "grid", "magrittr", "ggplot2"))
library(png)
library(tesseract)
library(magick)
library(boundingbox)
library(grid)
library(magrittr)
library(ggplot2)

img <- image_read("https://media.geeksforgeeks.org/wp-content/uploads/20190328185307/gfg28.png")
caja <- ocr_data(img)

#Cajas
caja <- as.data.frame(caja)
caja$bbox <- strsplit(caja$bbox, ",")
caja$xmin <- sapply(caja$bbox, function(x) as.numeric(x[1]))
caja$ymin <- sapply(caja$bbox, function(x) as.numeric(x[2]))
caja$xmax <- sapply(caja$bbox, function(x) as.numeric(x[3]))
caja$ymax <- sapply(caja$bbox, function(x) as.numeric(x[4]))

ggplot() +
  annotation_custom(rasterGrob(img)) +
  geom_rect(data = caja, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), color = "red",
            fill = NA) +
  geom_text(data = caja, aes(x = (xmin + xmax)/2, y = ymax+10 , label = word), color = "blue",
            size = 2) +
  theme_void() +
  scale_y_reverse()

caja$ymin[2:7] <- caja$ymin[2:7]-45 
caja$ymax[2:7] <- caja$ymax[2:7]-45
caja$xmin[2] <- caja$xmin[2]-10
caja$xmax[2] <- caja$xmax[2]-10
