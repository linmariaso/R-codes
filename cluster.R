library(ggplot2)
install.packages("dplyr")
library(dplyr)
library(corrplot)
library(corrr)
library(tidyverse)
library(cluster)
library(factoextra)

#Exploracion de datos
glimpse(USArrest)
summary(USArrest)

boxplot(USArrests)

#Check valores nulos
colSums(is.na(USArrests))
corrplot(cor(USArrests),method = "number")

data("USArrests")
dataUSA <- scale(USArrests)
head(dataUSA, n=5)
boxplot(dataUSA)

par(mfrow = c(1,2))
boxplot(dataUSA)
dataUSA <- na.omit(dataUSA)

#Cuantos cluster puedo generar
fviz_nbclust(dataUSA, kmeans, method = 'wss')
geom_vline(xintercept = 4, linetype = 2)
fviz_nbclust(dataUSA, kmeans, method = 'silhouette')
fviz_nbclust(dataUSA, kmeans, method = 'gap_stat')


set.seed(123)
kmUSA <- kmeans(dataUSA, 4, nstart = 10)
print(kmUSA)

# Mirar cuales son los cluster y cuales son los centroides
aggregate(USArrests,
          by = list(cluster = kmUSA$cluster),mean)
kmUSA$cluster

#Plot Cluster

fviz_cluster(kmUSA, data = dataUSA,
             palette=c("red","blue","green","orange"),
             ellipse.type = "euclid",
             start.plot = TRUE,
             repel = TRUE,
             ggtheme = theme_minimal()
             )

#matriz disimilaridad

install.packages("dendextend")
library(dendextend)

distUSA <- dist(dataUSA, method = "euclidean")

#Jerarquico
#demograma, metodo simple, promedio(average), medianas(median), centroide, complete
hierarchicalUSA <-hclust(distUSA, method = "complete")
plot(hierarchicalUSA, cex = 0.7, hang = -2)

#Metodo Ward
hierarchicalUSA <-hclust(distUSA, method = "ward.D2")
subUSA <- cutree(hierarchicalUSA, k=4)
table(subUSA)

plot(hierarchicalUSA, cex = 0.7)
rect.hclust(hierarchicalUSA, k = 4, border = 2:8)
