library(tidyverse)
library(e1071)
library(ggplot2)

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
  geom_point() +
  labs(title = 'Sepal Length vs Sepal Width')

index <- c(1:nrow(iris))

test.index <- sample(index, size = (length(index)/3))
train <- iris[test.index,]
test <- iris[-test.index,]

# Modelo Lineal
model_svm_l <- svm(Species~., data = train, kernel = 'linear')
table(prediction_svm_l = predict(model_svm_l, train), truth = train$Species)

# Modelo Polinomial
model_svm_p <- svm(Species~., data = train, kernel = 'polynomial')
table(prediction_svm_p = predict(model_svm_p, train), truth = train$Species)

# Modelo Radial
model_svm_r <- svm(Species~., data = train, kernel = 'radial')
table(prediction_svm_r = predict(model_svm_r, train), truth = train$Species)

# Modelo sigmoide
model_svm_s <- svm(Species~., data = train, kernel = 'sigmoid')
table(prediction_svm_s = predict(model_svm_s, train), truth = train$Species)

model_svm_l
model_svm_p
model_svm_r
model_svm_s