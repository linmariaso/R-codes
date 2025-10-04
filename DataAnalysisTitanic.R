install.packages("titanic")
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
library(titanic)

data("Titanic")
base <- data.frame(data("Titanic"))
str(Titanic)
summary(Titanic)

survived <- rpart(Survived~.,data = Titanic, cp = 0.02)
rpart.plot(survived,main = "Sobrevivientes del Titanic")
summary(survived)

data(ptitanic)
survived <- rpart(survived~.,data = ptitanic, cp = 0.02)
rpart.plot(survived,main = "Sobrevivientes del Titanic")
str(ptitanic)
ptitanic

prp(survived,extra = 7, prefix = "fraccion")
