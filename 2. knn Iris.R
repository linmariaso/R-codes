iris_knn <- iris
muestra_iris <- sample(1:150,50)
test_iris <- iris_knn[muestra_iris,]
learning_iris <- iris_knn[-muestra_iris,]
dim(test_iris)
dim(learning_iris)
install.packages("kknn")
suppressWarnings(suppressMessages(library(kknn)))

model_kknn <- train.kknn(Species~.,
                        data = learning_iris,
                        kmax = 9)
model_kknn

#Prediccion
prediccion_kknn <- predict(model_kknn, test_iris[,-5])
prediccion_kknn

matriz_verif <- table(test_iris[,5], prediccion_kknn)
matriz_verif

#Precision del modelo
precis_kknn <- sum(diag(matriz_verif))/sum(matriz_verif)
precis_kknn

plot(model_kknn)
