remotes::install_github("bnosac/image", subdir = "image.libfacedetection", build_vignettes = T)
library(magick)
install.packages("image.libfacedetection")
library(image.libfacedetection)

path_img <- "~/Ejercicios R/Taller8/handshake.jpg"
img <- image_read(path_img)

face <- image_detect_faces(img)
plot(face, img, border = "red", lwd = 2, col= "black")

###
install.packages("devtools")
library(devtools)
library(usethis)

yolo_img <- image_darknet_model(type = "detect", 
                                model = "tiny-yolo-voc.cfg",
                                weights = system.file(package = "image.darknet", "models","tiny-yolo-voc.weights"),
                                labels = system.file(package = "image.darknet", "include", "darknet", "data", "voc.names"))


path_dog <- "~/Ejercicios R/Taller8/dog.jpg"
img_dog <- image_read(path_dog)

install.packages("image.darknet")
library(image.darknet)

model_info <- image_darknet_detect(
  file = path_dog,
  object = yolo_img,
  threshold = 0.1
)