library(MASS)
library(neuralnet)
library(Amelia)
library(corrr)
library(corrplot)
library(PerformanceAnalytics)

#Base de datos Iris
data_iris <- iris

summary(data_iris)

data_iris_compl <- na.omit(data_iris)
dim(data_iris_compl)

missmap(data_iris.col = c('red','green'),
        y.at = 1, y.labels = "", legend = TRUE)

corrplot(cor(data_iris[,-5]), method = 'number', type = 'upper')

chart.Correlation(data_iris[,-5], histogram = FALSE, pch = 15)

color <- c('red', 'green', 'blue')[as.factor(data_iris[,5])]
plot(data_iris$Petal.Length * data_iris$Petal.Width, pch = 15, col = color)


#Seleccionar muestra
cant_filas <- nrow(data_iris)
cant_por <- round(cant_filas * 0.8)

set.seed(772)

#crear index
index <- sample(1:cant_filas, cant_por)

train <- data_iris[-index,]
test <- data_iris[index,]

neuronal_iris <- neuralnet(as.numeric(Species) ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                           train, hidden = 4)
plot(neuronal_iris)

neural_iris <- neuralnet(as.numeric(Species) ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                           train, hidden = c(5,3))
plot(neural_iris)

#n <- names(train)
#form <- as.formula(paste('Species~',
#                         paste(n[!n%in% 'Species'],
#                               collapse = '+')))

#NN <- neuralnet(form, data = train, hidden = c(5,3), linear.output = T)
#plot(NN)



neuronal_pred = compute(neuronal_iris,test[,-5])
neural_pred = compute(neural_iris,test[,-5])

pred_neuronal <- levels(data_iris$Species)[round(neuronal_pred$net.result)]
table_iris <- table(test[,5],pred_neuronal)

mse_neuronal <- sum(diag(table_iris))/sum(table_iris) #Tasa de aciertos
error_neuronal <- 1-mse_neuronal #Tasa de error

pred_neural <- levels(data_iris$Species)[round(neural_pred$net.result)]
table_iris_n <- table(test[,5],pred_neural)

mse_neural <- sum(diag(table_iris_n))/sum(table_iris_n) #Tasa de aciertos
error_neural <- 1-mse_neural #Tasa de error
