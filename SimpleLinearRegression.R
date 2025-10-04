# Regresion lineal simple

base <- read.table('http://verso.mat.uam.es/~joser.berrendero/datos/EdadPesoGrasas.txt', header = TRUE)
names(base)
pairs(base)
cor(base)

# Las variables edad y grasa son las que tienen mayor correlacion
# Cual es la variable dependiente? Grasas
# Cual es la variable independiente? Edad

regresion  <- lm(grasas ~ edad, data = base)
summary(regresion)

# Intercepto = 102.5751
# Edad = 5.3207
# Adjusted R square = 0.6882
# y = ax + b - Ecuacion de la recta
# grasas = a*edad + intercepto
# grasas = 5.3207*edad + 102.5751   Ajuste = 0.688

plot(base$edad,base$grasas,xlab = 'Edad',ylab = 'Grasas')
abline(regresion)