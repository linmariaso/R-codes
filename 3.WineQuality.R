library(ggplot2)
library(dplyr)
library(corrplot)
library(corrr)
library(tidyverse)
library(cluster)
library(factoextra)
library(fpc)
library(NbClust)
library(dendextend)

df <- read.csv("~/Ejercicios R/TallerIndividual/winequality-red.csv", sep = ",", header = TRUE)
summary(df)
glimpse(df)
boxplot(df)

colSums(is.na(df))
cor(df)
corrplot(cor(df),method = "number")

dataWine <- scale(df)
cor(dataWine)
boxplot(dataWine)
dataWine <- na.omit(dataWine)
corrplot(cor(dataWine),method = "number")

fviz_nbclust(dataWine, kmeans, method = 'wss') +
  geom_vline(xintercept = 4, linetype = 2)
fviz_nbclust(dataWine, kmeans, method = 'silhouette')

set.seed(123)
kmWine = kmeans(dataWine, 4, nstart = 25)
kmWine
fviz_cluster(object = kmWine, data = dataWine)

resultWine = data.frame(df, grupo = kmWine$cluster)
resultWine

