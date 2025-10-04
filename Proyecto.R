#Proyecto
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
library(FactoMineR)
library(psych)
library(ggpubr)
library(ggpointdensity)
library(GPArotation)
library(hrbrthemes)
library(kableExtra)
library(viridis)
library(gridExtra)
library(plotly)
library(rpart)
library(rpart.plot)
suppressWarnings(suppressMessages(library(kknn)))
library(cluster)
library(factoextra)
library(neuralnet)
library(PerformanceAnalytics)


########################## Data de Alzehimer

alz_data <- read.csv("~/Ejercicios R/Proyecto/alzheimers_disease_data.csv", sep = ",", header = TRUE)
str(alz_data)
names(alz_data)
summary(alz_data)

#Sacar correlaciones
correlation <- cor(alz_data[,2:34])

#Omitir valores nulos
alz_data <- na.omit(alz_data)

#Organizar datos para entrenar
alz <- data.frame(alz_data)
filterAlz <-  alz_data[,2:34]

plot(filterAlz$Diagnosis, filterAlz$MMSE)

#Sacar valores de train y test
ndata <- round(nrow(filterAlz))
nmuestra <- round(ndata*0.80)
set.seed(1234)
indices <- sample(1:ndata,nmuestra)
train <- filterAlz[indices,]
test <- filterAlz[-indices,]

## Modelo de arbol de decision
alzehimer <- rpart(Diagnosis~., data = train)
rpart.plot(alzehimer)

plot_alz <- filterAlz %>% select(MMSE, FunctionalAssessment, MemoryComplaints, 
                                 BehavioralProblems, ADL, Diagnosis)

plot(plot_alz)

#Prediccion Train
pred_train <-  predict(alzehimer,train[,-33], type = "vector")
result_train <- table(pred_train, train[,33])
tasa_acert <- (sum(diag(result_train)))/(sum(result_train))
round(tasa_acert,2)#[1] 0.02

#Prediccion Test
pred_test <-  predict(alzehimer,test[,-33], type = "vector")
result_test <- table(pred_test, test[,33])
tasa_acert_test <- (sum(diag(result_test)))/(sum(result_test))
round(tasa_acert_test,2) #[1] 0.02

## Modelo de KNN
model_kknn <- train.kknn(Diagnosis~.,
                         data = train,
                         kmax = 3)
model_kknn

#Prediccion KNN
prediccion_kknn <- predict(model_kknn, test[,-33])
prediccion_kknn

matriz_verif <- table(test[,33], prediccion_kknn)
matriz_verif

#Precision del modelo
precis_kknn <- sum(diag(matriz_verif))/sum(matriz_verif)
precis_kknn#[1] 0.4023256

plot(model_kknn)

#Modelo de cluster

fviz_nbclust(filterAlz, kmeans, method = 'silhouette')

set.seed(1234)
kmAlz <- kmeans(filterAlz, 2, nstart = 10)
print(kmAlz)

# Mirar cuales son los cluster y cuales son los centroides
aggregate(filterAlz,
          by = list(cluster = kmAlz$cluster),mean)
kmAlz$cluster


#Plot Cluster
fviz_cluster(kmAlz, data = filterAlz,
             palette=c("purple","orange"),
             ellipse.type = "euclid",
             start.plot = TRUE,
             repel = TRUE,
             ggtheme = theme_minimal()
)

#matriz disimilaridad
distAlz <- dist(filterAlz, method = "euclidean")

#Jerarquico
#demograma, metodo simple, promedio(average), medianas(median), centroide, complete
hierarchicalAlz <-hclust(distAlz, method = "complete")
plot(hierarchicalAlz, cex = 0.7, hang = -2)

#Metodo Ward
hierarchicalAlz <-hclust(distAlz, method = "ward.D2")
subAlz <- cutree(hierarchicalAlz, k=2)
table(subAlz)
plot(hierarchicalAlz, cex = 0.7)
rect.hclust(hierarchicalAlz, k = 2, border = 2:8)

#Prediccion de la regresion lineal multiple
lm.fit <- glm(Diagnosis~., data = train)
summary(lm.fit)
#Calculo del error cuadratico
pr.lm <- predict(lm.fit,test)
mse.lm <- sum(sum(pr.lm-test$Diagnosis)^2)/nrow(test)#[1] 1.18644

#Sacar maximos y minimos
max_x <- apply(filterAlz,2,max)
min_x <- apply(filterAlz,2,min)

#Escalar
scaled <- as.data.frame(scale(filterAlz, center = min_x, scale = max_x - min_x))

#Sacar test y train de la muestra escalada
train_sc <- scaled[indices,]
test_sc <- scaled[-indices,]

#Creacion de la formula
n <- names(train_sc)
form <- as.formula(paste('Diagnosis~',
                         paste(n[!n%in% 'Diagnosis'],
                               collapse = '+')))

#Red Neuronal
NN <- neuralnet(form, data = train_sc, hidden = c(5,2), linear.output = T)
plot(NN)

pr.NN <- compute(NN, test_sc[,1:32])

#Remover escala
pr.NN_ss <- pr.NN$net.result*(max(filterAlz$Diagnosis)-min(filterAlz$Diagnosis))+min(filterAlz$Diagnosis) 
test.r <- (test_sc$Diagnosis)*(max(filterAlz$Diagnosis)-min(filterAlz$Diagnosis))+min(filterAlz$Diagnosis)

mse.NN <- sum(sum(test.r-pr.NN_ss)^2)/(nrow(test_sc))
print(paste(mse.lm,mse.NN))#[1] "1.18643975538133 0.143767559977638"

plot(test$Diagnosis, pr.NN_ss, col = 'red', main = "Real vs prediccion NN", pch = 15, cex = 0.7)
points(test$Diagnosis, pr.lm, col = 'blue', pch = 15, cex = 0.7)
abline(0,1, lwd = 2)
legend('bottomright', legend = c("NN","lm"), pch=15, col = c('red','blue'), bty = 'n', cex = 0.95)


####Cluster Kmeans
dataAlz <- filterAlz[,-33]

alz_k = kmeans(dataAlz, centers = 2)
fviz_cluster(object = alz_k, data = dataAlz)

resultAlz = data.frame(filterAlz, grupo = alz_k$cluster)
resultAlz

table(resultAlz$Diagnosis, resultAlz$grupo)
resultAlz = resultAlz %>% mutate(grupo = recode(grupo,
                                                 "1"= "Alzehimer",
                                                 "2" = "No Alzehimer"))
resultAlz

resultFin = table(resultAlz$Diagnosis, resultAlz$grupo)
resultFin

sum(diag(resultFin))/sum(resultFin)#[1] 0.48953


