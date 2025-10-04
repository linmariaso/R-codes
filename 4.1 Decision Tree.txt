iris
data <- iris
summary(iris)
plot(iris)
iris_new <- iris[,-5]
cor(iris_new)
plot(iris_new$Petal.Length,iris_new$Petal.Width)
boxplot(iris_new,
        col = c("red","blue",
                "green"),
        main = "boxplot of iris dataset")
ndata <- nrow(data)
nmuestra <- round(ndata*0.80)
set.seed(1234)
indices <- sample(1:ndata,nmuestra)
training <- data[indices,]
test <- data[-indices,]
library(rpart)
# 1 Modelo
arbol1 <- rpart(Species~.,data = training)
library(rpart.plot)
rpart.plot(arbol1)
prediccion1 <- predict(arbol1,training[,-5], type = "class")
result <- table(prediccion1,training[,5])
result
tasa_acert <- (sum(diag(result)))/(sum(result))
round(tasa_acert,2)

pred_test <- predict(arbol1,test[,-5], type = "class")
result_test <- table(test[,5],pred_test)
result_test

tasa_acert_test <- (sum(diag(result_test)))/sum(result_test)
round(tasa_acert_test,2)
