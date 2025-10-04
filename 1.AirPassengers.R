library(ggplot2)
library(plotly)
library(lattice)
library(KernSmooth)
library(randomForest)
library(MPV)
AirPassengers
#Proyeccion - Regresion lineal
pasajeros <- data.frame(AirPassengers)
summary(AirPassengers)
plot(pasajeros, main = "Pasajeros", xlab = "Mes", ylab = "# Pasajeros")

graph <- plot_ly(y = AirPassengers, type = 'box', boxpoints = "all",
                 jitter = 0.2, pointpos = -2)

histogram <- plot_ly(x=AirPassengers, type = 'histogram')
histogram
linea <- plot_ly(y=AirPassengers, type = 'scatter', mode = 'lines')
linea

mes <- c(1:144)

regresion <- lm(AirPassengers~mes, data=pasajeros)
summary(regresion)

#Intercepto =  87.65278
#AirPassengers = 2.65718
#R Sqared = 0.8526
#y = 2.65718*x + 87.65278

ggplot(pasajeros, aes(x = mes, y = AirPassengers))+
  geom_point()+
  geom_smooth(method = 'lm', formula = y~x, se = FALSE, col = 'red')+
  theme_light()

