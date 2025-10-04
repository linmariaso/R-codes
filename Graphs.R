install.packages("ggplot2")
install.packages("plotly")
library(ggplot2)
library(plotly)


#Ejemplo grafica Barras
graph <- plot_ly(
  x = c("brooke","ivonne","mary"),
  y = c(35,18,22),
  name = "People",
  type = 'bar')

graph


#Programa 1- Linea

library(plotly)
x<- c(1:200)
y_ran <- rnorm(200,mean = 0)
base <- data.frame(x,y_ran)

figure <- plot_ly( base, x = ~x, y = ~y_ran, type = 'scatter', mode = 'line')
figure

# Programa 2 / Graficas de lineas y puntos
library(plotly)
x <- c(1:200)
var1 <- rnorm(200, mean = 5)
var2 <- rnorm(200, mean = 0)
var3 <- rnorm(200, mean = -5)

data <- data.frame(x,var1,var2,var3)

graph <- plot_ly(data, x = ~x, y = ~var1, name = 'Variable 1', type = 'scatter', mode = 'lines')
graph <- graph %>% add_trace( y = ~var2, name = 'Variable2', mode = 'lines+markers')
graph <- graph %>% add_trace( y = ~var3, name = 'Variable3', mode = 'markers')
graph


#Programa 3 - Grafico de PIE
city = c('Bogota','Medellin','Cali','Manizales')
y = c(20,40,15,22)

graf <- plot_ly(type = 'pie', labels = city, values = y, 
                textinfo = 'label+percent',
                insidetextorientation = 'radial')
graf

#Programa 4 - Grafico Box Plot

graph2 <- plot_ly( y = ~rnorm(75), type = 'box', boxpoints = 'all',
                   jitter = 0.2, pointpos = 0)
graph2

#Programa 5 - Histograma

graph3 <- plot_ly(x = ~rnorm(75), type = 'histogram')
graph3

graph4 <- plot_ly(y = ~rnorm(75), type = 'histogram')
graph4