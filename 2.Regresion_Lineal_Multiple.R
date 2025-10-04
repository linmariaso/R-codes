## Regresión lineal múltiple
## https://fhernanb.github.io/libro_regresion/rlm.html

# El modelo estadístico en regresión lineal múltiple 
# es una generalización del regresión lineal simple para  k
# covariables

install.packages("MPV")
library(MPV)
install.packages("scatterplot3d")
library(scatterplot3d)
install.packages("rgl")
library(rgl)

colnames(softdrink) <- c('tiempo', 'cantidad', 'distancia')
head(softdrink)
attach(softdrink)
scatterplot3d(x=cantidad, y=distancia, z=tiempo, pch=16, cex.lab=1,
              highlight.3d=TRUE, type="h", xlab='Cantidad de cajas',
              ylab='Distancia (pies)', zlab='Tiempo (min)')
plot3d(x=cantidad, y=distancia, z=tiempo, type='s', col='pink',
       xlab='Cantidad',
       ylab='Distancia (pies)',
       zlab='Tiempo (min)')

mod <- lm(tiempo ~ cantidad + distancia, data=softdrink)
summary(mod)