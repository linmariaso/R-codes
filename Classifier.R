trabajo <- c(100,40,60,70,70,60,80,90,20,50,60,50,30,20,20,10,80,90,20,70)
examen <- c(90,50,60,70,80,70,60,95,10,55,70,60,20,10,50,55,90,100,40,60)
clasificacion <- c(1,2,1,1,1,2,2,1,3,3,3,2,3,3,2,2,1,1,3,3)

datos <- data.frame(trabajo, examen, clasificacion)
plot(datos[,1:2], main = "Trabajo vs Examen", 
     xlab = "Trabajo", ylab = "Examen",
     col = datos$clasificacion,pch = 15)
legend("topright", legend = c("1", "2", "3"),
       pch = 15, col = c(1, 2, 3))
nalumn <- data.frame(trabajo = c(20,90,60,30,50,80,70), examen = c(30, 80, 90, 20, 70, 10,60))

plot(datos[,1:2], main = "Trabajo vs Examen", 
     xlab = "Trabajo", ylab = "Examen",
     col = datos$clasificacion,pch = 15)
legend("topright", legend = c("1", "2", "3"),
       pch = 15, col = c(1, 2, 3))
points(nalumn, col = "blue", pch=15, lwd =2)

#Entrenar
install.packages("class")
library(class)

model_knn <- knn(train = datos[,-3], 
                 test = nalumn,
                 cl = datos$clasificacion,
                 k = 3)

model_knn
