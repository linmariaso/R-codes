install.packages("magick")
library(magick)

img <- image_read('https://raw.githubusercontent.com/R-CoderDotCom/samples/main/bird.png')
print(img, info = FALSE)

info <- image_info(img)
info$width

#Escalar
scale <- image_scale(img, '200')
print(scale)

alto <- image_scale(img, 'x400')
print(alto)

#Recortar
image_trim(img)

#Parametro para recortar la imagen
recortar <- image_crop(img, geometry = '200x300+200+300')

#Rotar imagen
rotar <- image_rotate(img, degrees = 45)
print(rotar)

#Voltear imagen Vertical
voltear <- image_flip(img)
print(voltear)

#Voltear imagen horizontal
voltear_h <- image_flop(img)
print(voltear_h)

#Colorear el background (Fondo)
fondo <- image_background(img, color = 'red')
print(fondo)

#Agregar borde
borde <- image_border(img, color = 'blue', geometry = "4x5")
print(borde)

#Ponerle nombre a la imagen
letras <- image_annotate(img, "Lina", size = 50, 
                         gravity = "center", color = 'black')
print(letras)

#Distorsionar imagen
distort <- image_blur(img, radius = 10, sigma = 5)
print(distort)

#Agregar ruido a la imagen
ruido <- image_noise(img)
print(ruido)

#Invertir colores
invert <- image_negate(img)
print(invert)

#Dejarla en carboncillo
carboncillo <- image_charcoal(img)
print(carboncillo)

#Efecto Implosion
implosion <- image_implode(img, factor = 1)
print(implosion)

#Efecto pintura Oleo
oleo <- image_oilpaint(img, radius = 15)
print(oleo)

