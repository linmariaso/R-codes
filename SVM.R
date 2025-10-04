library(ggplot2)

#Semilla
set.seed(68)

x1 <- rnorm(n=10,mean = 2, sd=1)
x2 <- rnorm(n=10,mean = 2, sd=1)
obs <- data.frame(x_1 = c(x1,x1+2),
                  x_2 = c(x2,x2+2),
                  clase = rep(c(1,-1), each = 10))
obs$clase <- as.factor(obs$clase)

#Plotear
ggplot()+
  geom_point(data = obs, aes(x=x_1, y=x_2,color = clase), size = 3) +
  geom_abline(intercept = 8, slope = -2) +
  theme_bw() +
  labs(title = "SVM")


set.seed(10111)
coord <- matrix(rnorm(40), 20, 2)
colnames(coord) <- c("x1","x2")

#Sacar los valores representados entre -1 y 1
y <- c(rep(-1,10),rep(1,10)) 

coord[y==1,] <- coord[y==1,] +1
varplot <- data.frame(coord,y)

ggplot(data = varplot,aes(x = x1, y = x2, color = as.factor(y))) +
  geom_point(size = 5) +
  theme_bw() +
  theme(legend.position = 'NONE')
#Los modelos no son separables segun esta imagen

library(e1071)
varplot$y <- as.factor(varplot$y)
model_svm <- svm(formula= y~x1+x2,
                 data=varplot,
                 kernel = 'linear', cost = 10, scale = FALSE)
summary(model_svm)

plot(model_svm, varplot)
