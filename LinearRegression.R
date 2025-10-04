# Programa 6 
install.packages("MPV")
install.packages("lattice")
install.packages("KernSmooth")
install.packages("randomForest")
library(MPV)

#Traer datos
table.b3[22:26,]
datos <- table.b3[-c(23:25),]

#1. Backwards - y dependiente

modelf <- lm(y~., data=datos)
summary(modelf)

# y = -0.07*x1 - 0.7*x2 + 0.116*x3 + ... + 0.50*x11 + 19.25  r ajuste 0.69

install.packages("MASS")
library(MASS)
#Criterio de informacion AIC

backward <- stepAIC(modelf, trace = TRUE, direction = 'backward')

backward$anova
summary(backward)

# y = 2.26*x5 + 0.21*x8 - 0.009*x10 + 5.99  r de ajuste= 0.75
# x5 es la relación con el eje trasero
# x8 es la longitud total
# x10 es el peso

modele <- lm(y~ 1, data = datos)

form <- formula(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 +x8 + x9 + x10 + x11)
forward <- stepAIC(modele, trace = FALSE, direction = 'forward', scope = form)
forward$anova
summary(forward)

# y = -0.045*x1 +32.9 r ajuste 0.72
# x1 es el desplazamiento

#Tercer metodo
modelb <- stepAIC(modele, trace= FALSE, direction = 'both', scope = form)
modelb$anova
summary(modelb)

pairs(datos)
