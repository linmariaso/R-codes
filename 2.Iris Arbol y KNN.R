iris_data <- iris
muestra_iris <- sample(1:150,50)
test_iris <- iris_data[muestra_iris,]
learning_iris <- iris_data[-muestra_iris,]
iris_new <- learning_iris[,-5]

plot(iris_new$Petal.Length,iris_new$Petal.Width)
boxplot(iris_new,
        col = c("red","blue",
                "green"),
        main = "boxplot of iris dataset")

library(rpart)
# 1 Modelo Arbol de decision
arbol1 <- rpart(Species~.,data = learning_iris)
library(rpart.plot)
rpart.plot(arbol1)
prediccion1 <- predict(arbol1,learning_iris[,-5], type = "class")
result <- table(prediccion1,learning_iris[,5])
result
tasa_acert <- (sum(diag(result)))/(sum(result))
round(tasa_acert,2)
#Tasa acert = 0.99

pred_test <- predict(arbol1,test_iris[,-5], type = "class")
result_test <- table(test_iris[,5],pred_test)
result_test

tasa_acert_test <- (sum(diag(result_test)))/sum(result_test)
round(tasa_acert_test,2)
#Tasa acert test = 0.9

# 2 Modelo KNN 
suppressWarnings(suppressMessages(library(kknn)))

model_kknn <- train.kknn(Species~.,
                         data = learning_iris,
                         kmax = 9)
model_kknn

#Prediccion KNN
prediccion_kknn <- predict(model_kknn, test_iris[,-5])
prediccion_kknn

matriz_verif <- table(test_iris[,5], prediccion_kknn)
matriz_verif

#Precision del modelo KNN
precis_kknn <- sum(diag(matriz_verif))/sum(matriz_verif)
precis_kknn
#Tasa acert = 0.9

plot(model_kknn)
