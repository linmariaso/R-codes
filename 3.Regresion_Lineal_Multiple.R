# https://fhernanb.github.io/libro_regresion/selec.html

library(MPV)  # Aqui estan los datos
table.b3[22:26, ] # Can you see the missing values?

datos <- table.b3[-c(23, 25), ]

# Aplicación del método backward

full.model <- lm(y ~ ., data=datos)
summary(full.model)
library(MASS)  # Para poder usar la funcion stepAIC
modback <- stepAIC(full.model, trace=TRUE, direction="backward")
modback$anova
summary(modback)

# Aplicación del método forward
empty.model <- lm(y ~ 1, data=datos)
horizonte <- formula(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11)
modforw <- stepAIC(empty.model, trace=FALSE, direction="forward", scope=horizonte)
modforw$anova
summary(modforw)|

modforw <- update(modforw, y ~ x1)
summary(modforw)

# Aplicación del método both
modboth <- stepAIC(empty.model, trace=FALSE, direction="both", scope=horizonte)
modboth$anova