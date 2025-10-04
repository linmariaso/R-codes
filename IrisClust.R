library(dplyr)
library(cluster)
library(ggplot2)
library(factoextra)
install.packages("fpc")
library(fpc)
install.packages("NbClust")
library(NbClust)

data(iris)
dim(iris)
head(iris,3)
table(iris$Species)
boxplot(iris)

par(mfrow= c(2,2))
boxplot(iris$Sepal.Length)
boxplot(iris$Sepal.Width)
boxplot(iris$Petal.Length)
boxplot(iris$Petal.Width)

ggplot(iris, aes(x= Species, y= Sepal.Length)) +
  geom_boxplot(aes(fill = Species))

ggplot(iris, aes(x= Species, y= Sepal.Length)) +
  geom_boxplot() +
  geom_jitter(aes(color = Species))

dataIris = iris[,-5]
dataIris = scale(dataIris)

set.seed(15978)
iris_k = kmeans(dataIris, centers = 3)
fviz_cluster(object = iris_k, data = dataIris)

resultIris = data.frame(iris, grupo = iris_k$cluster)
resultIris

table(resultIris$Species, resultIris$grupo)
resultIris = resultIris %>%mutate(grupo = recode(grupo,
                                                 "1"= "Virginica",
                                                 "2" = "Setosa",
                                                 "3" = "Versicolor"))
resultIris
resultFin = table(resultIris$Species, resultIris$grupo)
resultFin

sum(diag(resultFin))/sum(resultFin)
