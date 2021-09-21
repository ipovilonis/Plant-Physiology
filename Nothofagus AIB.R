#---
#  FISIOLOGÍA VEGETAL - PRÁCTICO Nothofagus
#  HORMONAS VEGETALES: Efecto del AIB en enrraizamiento de estacas de roble colorado.
#---

# 1) Defina el objetivo del ensayo.
# 
# 2) Identifique: Variable explicatoria, variable respuesta, ¿Qué tipo de variables son?, unidad experimental, unidad observacional, número de muestras por cada tratamiento (n), diseño del experimento.
# 
# 3) Cargue la tabla a Rstudio y obtenga las medidas resumen.
# 
# 4) Realice un gráfico exploratorio de la información.
# 
# 5) Plantee las hipótesis a poner a prueba.
# 
# 6) Ponga a prueba las hipótesis a partir de un análisis de varianza.
# 
# 7) Realice las pruebas a posteriori (Test de Tukey).
# 
# 8) Realice un gráfico de barras indicando con letras las diferencias (si las hubo) entre tratamientos.
# 
# 9) Conclusiones: magnitud del efecto.

# Diseño: completamente aleatorizado. ANOVA de 1 factor.
# Tipo de estudio:manipulativo
# U. Experimental:cada estaca
# U. Observacional:cada estaca
# VE/s: Dosis de AIB (ácido indol butírico)
# Tipo de variable: Cuantitativa -> cualitativa (anova)
# VR: Longitud de la raíz (cm)
# Tipo de variable: Cuantitativa continua
# Tratamiento/s: 0, 0.5, 1.5
# Factor/es: dosis AIB
# Niveles: 3

# Paquetes usados - si no está instalado usar: insall.packages("namepackage")
library(Rmisc)
library(lattice)
library(plyr)
library(ggplot2)
library(car)
library(emmeans)
library(agricolae)

## Datos Nothofagus -----

aib<-read.delim("AIB.txt")

### Verificar y/o convertir tipos de datos
names(aib)
head(aib)
str(aib)
aib$dosis_f <- as.factor(aib$dosis)
str(aib)
attach(aib)

## Estadística descriptiva -----

library(Rmisc)
library(lattice)
library(plyr)
# summarySE provides the standard deviation, standard error of the mean, and a (default 95%) confidence interval
est <- summarySE(aib, measurevar="longitud", groupvars=c("dosis_f"))
est

library(ggplot2)
# Barras de error representan el desv?o estandar
ggplot(est, aes(x=dosis_f, y=longitud)) + 
  stat_summary(fun = "mean", colour = "red", size = 4, geom = "point") +
  ylab("longitud") + 
  geom_errorbar(aes(ymin=longitud-sd, ymax=longitud+sd), width=.2, position=position_dodge(.9)) 

# Barras de error representan el intérvalo de confianza
ggplot(est, aes(x=dosis_f, y=longitud)) + 
  stat_summary(fun = "mean", colour = "red", size = 4, geom = "point") +
  ylab("longitud") + 
  geom_errorbar(aes(ymin=longitud-ci, ymax=longitud+ci), width=.2, position=position_dodge(.9)) 

### Gráficos de puntos
ggplot(aib) +
  aes(x = dosis_f, y = longitud) +
  geom_jitter(width = 0.1) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", color = "red")

### Gráficos boxplot
ggplot(aib) +
  aes(x = dosis_f, y = longitud, colour=dosis_f) +
  geom_boxplot()

# Hipótesis a poner a prueba:
# H0: "No hay diferencias entre dosis aplicadas de AIB"

## Modelo lineal -----
m_aov <- aov(longitud ~ dosis_f, aib)
m_aov

# Supuestos ----

## Residuos y predichos
fitted(m_aov)
resid(m_aov)
rstandard(m_aov)

# Gráficos de diagnóstico
par(mfcol = c(2,2)) # para obtener todo en un grafico
plot(m_aov)
par(mfcol = c(1,1))
# dev.off() para reiniciar los parametros

# Homogeneidad
plot(m_aov, which = 1)
leveneTest(m_aov)

# Normalidad
plot(m_aov, which = 2)
shapiro.test(resid(m_aov))

# No hay evidencias para aceptar que no se cumplen los supuestos del modelo.

# resumen del modelo
summary.lm(m_aov)

# ANOVA ----
anova(m_aov)

# Hay diferencias significativas entre tratamientos.

# Tukey ----

# emmeans
tukey2 <- contrast(lsm, method = 'pairwise', adjust = "tukey") 
tukey2
confint(tukey2)
plot(confint(tukey2)) + 
  geom_vline(xintercept = 0)
multcomp::cld(lsm, adjust = "tukey")
pwpp(tukey2)

# agricolae
HSD.test(m_aov, trt = "dosis_f", console = T)

# MAGNITUD DEL EFECTO ----

# Absoluto = 4,92 - 1,50 = 3,42 cm. "El incremento en la longitud de la raíz por el tratamiento con AIB fue de 3,42cm en promedio (IC=95%)"
# Relativo = 3,42 / 1,50 = 2,28 --x100--> 228%.  "El incremento en la longitud de la raíz por el tratamiento con AIB fue de 228% en promedio (IC=95%)"
