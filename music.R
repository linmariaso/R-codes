library(ggplot2)
library(dplyr)
library(corrplot)
library(corrr)
library(tidyverse)
library(cluster)
library(factoextra)
install.packages("fpc")
library(fpc)
library(NbClust)
library(dendextend)
install.packages("FactoMineR")
library(FactoMineR)
install.packages("psych")
library(psych)
install.packages("ggpubr")
library(ggpubr)
install.packages("ggpointdensity")
library(ggpointdensity)
install.packages("GPArotation")
library(GPArotation)
install.packages("hrbrthemes")
library(hrbrthemes)
install.packages("kableExtra")
library(kableExtra)
install.packages("viridis")
library(viridis)
install.packages("gridExtra")
library(gridExtra)

dataMusic <- read.csv("~/Ejercicios R/Taller4/music_genre.csv", sep = ",", header = TRUE)
dataMusic$artist_name <- as.factor(dataMusic$artist_name)
dataMusic$music_genre <- as.factor(dataMusic$music_genre)

str(dataMusic)
view(dataMusic)

levels(dataMusic$music_genre)
names(dataMusic)

data_num <- dataMusic%>%
  select(acousticness,danceability,energy,
         instrumentalness,loudness,speechiness,
         valence,music_genre,popularity,
         artist_name, track_name)

view(data_num)
dim(data_num)

data_num <- na.omit(data_num)
dim(data_num)

corrplot(cor(data_num[,1:7]), method = 'number',
         order = 'AOE', type = 'upper')

### PCA
pca <- princomp(data_num[,1:7], cor = TRUE)
fviz_screeplot(pca, addlabel = TRUE)

pca$loadings
fviz_pca_var(pca)
data_num$PC1 <- pca$scores[,1]
data_num$PC2 <- pca$scores[,2]

data_num %>%
  filter(music_genre%in% c('Classical','Rock','Jazz','Electronic')) %>%
  ggplot(aes(PC1,PC2, col = music_genre)) +
  geom_point(alpha = 0.5) +
  ggtitle("PCA genero clasico y rock")+
  scale_color_discrete("Genero", labels = c("Classical","Rock","Jazz","Electronic"), 
                       type = c("blue","red","green","orange")) +
  theme_ipsum()

data_num %>%
  filter(music_genre%in% c('Classical','Rock')) %>%
  ggplot(aes(PC1,PC2, col = music_genre)) +
  geom_point(alpha = 0.5) +
  ggtitle("PCA genero clasico y rock")+
  scale_color_discrete("Genero", labels = c("Classical","Rock"), 
                       type = c("blue","red")) +
  theme_ipsum()

data_num %>% filter(music_genre =='Classical') %>%
  arrange(-PC1,PC2) %>%
  select(PC1,PC2,artist_name,track_name) %>%
  head()