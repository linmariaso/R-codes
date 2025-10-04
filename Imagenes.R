library(magick)

img <- image_read("https://jeroen.github.io/images/frink.png")
print(img)
image_border(img, "red")
image_background(img, "hotpink")
image_border(image_trim(img), "red")
image_crop(img, "100x200")
image_scale(img, "50")
image_rotate(img, 45)

##############################################################

library(png)
library(tesseract)
library(magick)
library(boundingbox)
library(grid)
library(magrittr)
library(ggplot2)

img <- image_read("https://concepto.de/wp-content/uploads/2020/11/parrafo-ejemplo-e1606250188911.jpg")
print(img)
text <- ocr(img)
print(text)

analisis <- img %>% 
  image_resize("2000x") %>%                # reescalamos la imagen
  image_convert(type = "Grayscale") %>%    # convertimos en escala de grises
  image_trim(fuzz = 40) %>%                # cortar 
  image_write(format = "png", density = "300x300") %>% #convertimos en formato png la imagen
  tesseract::ocr()


caja <- ocr_data(img)
caja <- as.data.frame(caja)
caja$bbox <- strsplit(caja$bbox, ",")
caja$xmin <- sapply(caja$bbox, function(x) as.numeric(x[1]))
caja$ymin <- sapply(caja$bbox, function(x) as.numeric(x[2]))
caja$xmax <- sapply(caja$bbox, function(x) as.numeric(x[3]))
caja$ymax <- sapply(caja$bbox, function(x) as.numeric(x[4]))

ggplot() +
  annotation_custom(rasterGrob(analisis)) +
  geom_rect(data = caja, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), color = "red",
            fill = NA) +
  geom_text(data = caja, aes(x = (xmin + xmax)/2, y = ymax+10 , label = word), color = "blue",
            size = 2) +
  theme_void() +
  scale_y_reverse()
