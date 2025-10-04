library(ggplot2)
library(patchwork)
library(corrplot)
install.packages("devtools")
library(devtools)
library(ggbiplot)
data(iris)
#Encabezados
head(iris)
#Histograma
par(mfrow = c(2,2))

#distribucion info
hist(iris$Sepal.Length, breaks = 20)
hist(iris$Sepal.Width, breaks = 20)
hist(iris$Petal.Length, breaks = 20)
hist(iris$Petal.Width, breaks = 20)

#Distribucion del primerocon respecto a las salidas
sepal_l <- ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_boxplot(aes(fill = Species))

sepal_w <- ggplot(iris, aes(x = Species, y = Sepal.Width)) +
  geom_boxplot(aes(fill = Species))

petal_l <- ggplot(iris, aes(x = Species, y = Petal.Length)) +
  geom_boxplot(aes(fill = Species))

petal_w <- ggplot(iris, aes(x = Species, y = Petal.Width)) +
  geom_boxplot(aes(fill = Species))

sepal_l +
sepal_w +
petal_l +
petal_w

#Distribucion de graficas iris
plot(iris[,-5])

#Correlacion
corrplot(cor(iris[,-5]), type = 'upper', method = 'number')

#Hallar PCA - Bajar dimensionalidad - Tomar todas menos la columna 5
iris_pca <- prcomp(iris[,-5], scale = TRUE)
print(iris_pca)

summary(iris_pca)
#Sacar la desviacion estandar
iris_pca$sdev
#Plot de la varianza
plot(iris_pca, type = 'l')

ggbiplot(iris_pca, obs.scale = 1, var.scale = 1,
        groups = iris$Species, ellipse = TRUE, ellipse.prob = 0.68) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')
