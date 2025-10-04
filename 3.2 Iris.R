library(ggplot2)
library(GGally)
library(Amelia)
library(corrplot)
library(factoextra)
library(ggbiplot)


data("USArrests")
head(USArrests)
plot(USArrests)

#Densidad de probabilidad
ggpairs(USArrests)

#Histograma
hist(USArrests$Murder,
     probability = TRUE,
     name = "Histograma y Densidad de Probabilidad", 
     xlab = "Arrestos por cada 100 mil Habitantes")

lines(density(USArrests$Murder), col = 'red', lwd = 2)

summary(USArrests)

missmap(USArrests, col = c('red','green'),
        y.at = 1 , y.labels = "", legend = TRUE)

corrplot(cor(USArrests), method = 'number', type = 'upper')

head(USArrests)

#Matriz de distancia por metodo euclidiano

usa <- scale(USArrests, center = TRUE, scale = TRUE) 
matriz_distancia <- dist(x = usa, method = 'euclidean')
round(as.matrix(matriz_distancia)[1:5,1:5],2)
dim(matriz_distancia) 

pca_USA <- prcomp(usa)

#Variables
names(pca_USA)
#centros
pca_USA$center
pca_USA$rotation
head(pca_USA$x)

plot(pca_USA, type = 'lines')

biplot(pca_USA, obs.scale = 1, var.scale = 1, 
         ellipse = TRUE, ellipse.prob = 0.68) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')
