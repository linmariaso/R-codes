library(neuralnet)

n1 <- c(3,5,7,8,2,1,5)
n2 <- c(0,1,7,9,10,5,8)
salida <- c(0,0,1,1,1,0,1)
calif <- data.frame(n1,n2,salida)

# Ecuacion: salida ~ n1 + n2
#Crear la red
red <- neuralnet(salida~n1+n2,
                 data=calif, hidden = 1,
                 act.fct = "logistic",
                 linear.output = FALSE)
plot(red)

#Crear dataframe de test
n1test <- c(5,1.5)
n2test <- c(5.8,0)
test <- data.frame(n1test,n2test)

#Usar la red creada para predecir

npred = compute(red,test)
prob <- npred$net.result

out <- ifelse(prob>0.6,1,0)

#

library(MASS)
library(neuralnet)
library(Amelia)
library(corrr)
library(corrplot)
library(PerformanceAnalytics)
set.seed(500)
Boston <- Boston

#Limpieza de datos y analisis exploratorio
str(Boston)
head(Boston)
summary(Boston)
missmap(Boston,col = c("red","green"),y.at = 1, y.labels = "",legend = TRUE)
corrplot(cor(Boston),method = 'number', type = 'upper')
Boston <- Boston[,1:4]
chart.Correlation(Boston, histogram = FALSE, pch =15)

index <- sample(1:nrow(Boston), round(0.75*nrow(Boston)))
train <- Boston[index,]
test <- Boston[-index,]

#Prediccion de la regresion lineal multiple
lm.fit <- glm(medv~., data = train)
summary(lm.fit)
#Calculo del error cuadratico
pr.lm <- predict(lm.fit,test)
mse.lm <- sum(sum(pr.lm-test$medv)^2)/nrow(test)

#Sacar maximos y minimos
max_x <- apply(Boston,2,max)
min_x <- apply(Boston,2,min)

#Escalar
scaled <- as.data.frame(scale(Boston, center = min_x, scale = max_x - min_x))

train_sc <- scaled[index,]
test_sc <- scaled[-index,]

n <- names(train_sc)
form <- as.formula(paste('medv~',
                         paste(n[!n%in% 'medv'],
                               collapse = '+')))

NN <- neuralnet(form, data = train_sc, hidden = c(5,3,2), linear.output = T)
plot(NN)

#Predecir
pr.NN <- compute(NN, test_sc[,1:13])

#Quitar escala
pr.NN_sc <- pr.NN$net.result*(max(Boston$medv)-min(Boston$medv))+min(Boston$medv) 
test.r <- (test_sc$medv)*(max(Boston$medv)-min(Boston$medv))+min(Boston$medv)

#Error
mse.NN <- sum(sum(test.r-pr.NN_sc)^2)/(nrow(test_sc))
print(paste(mse.lm,mse.NN))

plot(test$medv, pr.NN_sc, col = 'red', main = "Real vs prediccion NN", pch = 15, cex = 0.7)
abline(0,1, lwd = 2)
legend('bottomright', legend = "NN", pch=15, col = 'red', bty = 'n')

plot(test$medv, pr.lm, col = 'blue', main = "Real vs prediccion NN", pch = 15, cex = 0.7)
abline(0,1, lwd = 2)
legend('bottomright', legend = "lm", pch=15, col = 'blue', bty = 'n', cex = 0.95)

plot(test$medv, pr.NN_sc, col = 'red', main = "Real vs prediccion NN", pch = 15, cex = 0.7)
points(test$medv, pr.lm, col = 'blue', pch = 15, cex = 0.7)
abline(0,1, lwd = 2)
legend('bottomright', legend = c("NN","lm"), pch=15, col = c('red','blue'), bty = 'n', cex = 0.95)