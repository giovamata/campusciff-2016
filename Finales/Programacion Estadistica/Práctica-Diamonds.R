############################ ANALISIS ESTADISTICO - Master BI y BD  ###############################

# Hacer uso del dataset "diamonds" que contendra el precio (entre otras variables interesantes) de unos 54.000 diamantes.
#
# Objetivo : realizar distintos tipos de analisis estadistico de sus variables para intentar
# averiguar algun tipo de comportamiento oculto aparentemente en los datos. 
#
# Para ello os marco los siguientes pasos: tipos de variables, medidas de posicion central, medidas de dispersion, 
# distribucion y relacion entre ellas, mas analisis de regresion
#
# Los diferentes indicadores presentes en el dataset "diamonds" son los siguientes:
# price: Precio en dolares americanos
# carat: peso del diamante
# cut: calidad del corte (Fair, Good, Very Good, Premium, Ideal)
# colour: color del diamante (desde D el mejor hasta J el peor)
# clarity: mide como de claro es el diamante (desde el peor I1, SI2, SI1, VS2, VS1, VVS2, VVS1, hasta el mejor IF)
# x: longitud en mm 
# y: ancho en  mm 
# z: profundidad en mm 
# depth: porcentaje total de profundidad 
# table: anchura de la parte superior de diamante con relación al punto más ancho 


# Responde cada bloque cubriendo al menos lo indicado:
# asignando DF Diamonds
library(ggplot2)
library(sampling)
library(SamplingStrata)
library(e1071)
library(moments)
library(prettyR)
library(plyr)
library(nortest)
library(corrplot)
library(lawstat)
library(car)

library(ggplot2)
dt<-as.data.frame(diamonds)

head(dt)
attach(dt)

# ******************************** Punto 1: Muestra representativa ********************************
# usamos paquete sampling y seleccionamos el un porcentaje de cada corte
table(dt$cut)
Datoscorte<-data.frame(table(cut))
Muestra=0.09

# Obtenemos los varlores a obtener para cada corte
MuestraFair<-round(Datoscorte[1,2]*Muestra)
MuestraGood<-round(Datoscorte[2,2]*Muestra)
MuestraVGood<-round(Datoscorte[3,2]*Muestra)
MuestraPremium<-round(Datoscorte[4,2]*Muestra)
MuestraIdeal<-round(Datoscorte[5,2]*Muestra)

library(sampling)
estratos <- strata(dt[order(dt$cut),], stratanames = c("cut"), size = c(MuestraFair,MuestraGood,MuestraVGood,MuestraPremium,MuestraIdeal), method = "srswor")
dtm <- getdata( dt, estratos )
drop <- c("ID_unit","Prob","Stratum")
dtm = dtm[,!(names(dtm) %in% drop)]

# Original
table(dt$cut)
# Muestra
table(dtm$cut)

# ******************************** Punto 2: Análisis de las variables ********************************
# Análisis descriptivo de las variables: Tipo de variable, distribución y representación
# Detección de casos atípicos y su tratamiento

# FUnciones a utilizar en  el analisis de datos

str(dt)

# 'data.frame':	53940 obs. of  10 variables:
# $ carat  : num  0.23 0.21 0.23 0.29 0.31 0.24 0.24 0.26 0.22 0.23 .             ---> variable cuantitativa
# $ cut    : Ord.factor w/ 5 levels "Fair"<"Good"<..: 5 4 2 4 2 3 3 3 1 3 ...     ---> variable cualitativa, tiene 5 categorias
# $ color  : Ord.factor w/ 7 levels "D"<"E"<"F"<"G"<..: 2 2 2 6 7 7 6 5 2 5 ...   ---> variable cualitativa, tiene 7 categorias
# $ clarity: Ord.factor w/ 8 levels "I1"<"SI2"<"SI1"<..: 2 3 5 4 2 6 7 3 4 5 ...  ---> variable cualitativa, tiene 8 categorias
# $ depth  : num  61.5 59.8 56.9 62.4 63.3 62.8 62.3 61.9 65.1 59.4 ...           ---> variable cuantitativa
# $ table  : num  55 61 65 58 58 57 57 55 61 61 ...                               ---> variable cuantitativa
# $ price  : int  326 326 327 334 335 336 336 337 337 338 ...                     ---> variable cuantitativa
# $ x      : num  3.95 3.89 4.05 4.2 4.34 3.94 3.95 4.07 3.87 4 ...               ---> variable cuantitativa
# $ y      : num  3.98 3.84 4.07 4.23 4.35 3.96 3.98 4.11 3.78 4.05 ...           ---> variable cuantitativa
# $ z      : num  2.43 2.31 2.31 2.63 2.75 2.48 2.47 2.53 2.49 2.39 ...           ---> variable cuantitativa

# Vemos los datos del dataframe y con solo verlo el resultado y a simple viste se ve que tiene outliers, por lo que se procede a eliminarlos
summary(dtm)

dta <- dtm
dta <- na.omit(dta)
# Para eliminar outliers y analizar variables cualitativas les daremos valores numericos
# Corte
revalue(dta$cut, c("Fair" = 1)) -> dta$cut
revalue(dta$cut, c("Good" = 2)) -> dta$cut
revalue(dta$cut, c("Very Good" = 3)) -> dta$cut
revalue(dta$cut, c("Premium" = 4)) -> dta$cut
revalue(dta$cut, c("Ideal" = 5)) -> dta$cut
dta$cut <- as.numeric(as.character(dta$cut))
summary(dta$cut)

# Color
revalue(dta$color, c("D" = 1)) -> dta$color
revalue(dta$color, c("E" = 2)) -> dta$color
revalue(dta$color, c("F" = 3)) -> dta$color
revalue(dta$color, c("G" = 4)) -> dta$color
revalue(dta$color, c("H" = 5)) -> dta$color
revalue(dta$color, c("I" = 6)) -> dta$color
revalue(dta$color, c("J" = 7)) -> dta$color
dta$color <- as.numeric(as.character(dta$color))
summary(dta$color)

# Claridad
revalue(dta$clarity, c("I1" = 1)) -> dta$clarity
revalue(dta$clarity, c("SI2" = 2)) -> dta$clarity
revalue(dta$clarity, c("SI1" = 3)) -> dta$clarity
revalue(dta$clarity, c("VS2" = 4)) -> dta$clarity
revalue(dta$clarity, c("VS1" = 5)) -> dta$clarity
revalue(dta$clarity, c("VVS2" = 6)) -> dta$clarity
revalue(dta$clarity, c("VVS1" = 7)) -> dta$clarity
revalue(dta$clarity, c("IF" = 8)) -> dta$clarity
dta$clarity <- as.numeric(as.character(dta$clarity))
summary(dta$clarity)

#Ahora procedemos a eliminacion de outliers 
ocarat =c(quantile(dta$carat,.25)-1.5*IQR(dta$carat), quantile(dta$carat,.75)+1.5*IQR(dta$carat))
odepth = c(quantile(dta$depth,.25)-1.5*IQR(dta$depth), quantile(dta$depth,.75)+1.5*IQR(dta$depth))
otable = c(quantile(dta$table,.25)-1.5*IQR(dta$table), quantile(dta$table,.75)+1.5*IQR(dta$table))
oprice = c(quantile(dta$price,.25)-1.5*IQR(dta$price), quantile(dta$price,.75)+1.5*IQR(dta$price))
ox = c(quantile(dta$x,.25)-1.5*IQR(dta$x), quantile(dta$x,.75)+1.5*IQR(dta$x))
oy = c(quantile(dta$y,.25)-1.5*IQR(dta$y), quantile(dta$y,.75)+1.5*IQR(dta$y))
oz = c(quantile(dta$z,.25)-1.5*IQR(dta$z), quantile(dta$z,.75)+1.5*IQR(dta$z))
outl<- rbind(ocarat, odepth, otable, oprice, ox, oy, oz)

dtno <- dta[dta$carat>outl[1,1] & dta$carat<outl[1,2],]
dtno <- dtno[dtno$depth>outl[2,1] & dtno$depth<outl[2,2],]
dtno <- dtno[dtno$table>outl[3,1] & dtno$table<outl[3,2],]
dtno <- dtno[dtno$price>outl[4,1] & dtno$price<outl[4,2],]
dtno <- dtno[dtno$x>outl[5,1] & dtno$x<outl[5,2],]
dtno <- dtno[dtno$y>outl[6,1] & dtno$y<outl[6,2],]
dtno <- dtno[dtno$z>outl[7,1] & dtno$z<outl[7,2],]

# Con los datos que nos quedamos, sin outliers y muestra Estratificada
str(dtno)

# Se utlizaran graficos en este analisis, definimos 2 graficos por pantalla
par(mfrow=c(1,2))

# *** Analisis variable CARAT ***
# Vemos la distribucion de los valores de la variable y graficamos.
summary(dtno$carat)
boxplot(dtno$carat, horizontal = F)
hist(dtno$carat)
# Se observa en el summary que el grafico esta tirado a la izquierda (q1 y mediana se acercan), y que tiene una cola a la derecha,
# confirmandose en el histograma.

plot(density(dtno$carat))
qqnorm(dtno$carat)
qqline(dtno$carat, col=2)

skew(dtno$carat) # es positiva, lo que significa que tiene cola hacia la derecha
kurtosis(dtno$carat) # 
shapiro.test(dtno$carat)

# *** Conclucion variable CARAT ***
# Despues de eliminar los outliers, se puede observar que la media y mediana tienen valores muy cercanos,
# el boxplot nos muestra ausencia de outliers y ls histograma muestra la cola de valores a la dercha.
# Los graficos de frequencia y densidad, se observa un comportamiento variable (disminuyendo y aumentando), con lo cual 
# NO parece una distribucion normal, comprobandose con el grafico de Qplot. LOs valores de Skew confirman que NO hay una distribucion normal
# y el de Kurtosis que tiene una colas. COntrastamos con el shapiro que  tiene un valor < 0.05, por lo que 
# Podemos concluir que NO tienen una distribucion normal

# *** Analisis variable CUT ***
summary(dtno$cut)
boxplot(dtno$cut, horizontal = F)
hist(dtno$cut)
# Se observa en el summary que la media y la mediana tienen valore similares y que a mayor calidad mas frecuencia
plot(density(dtno$cut))
qqnorm(dtno$cut)
qqline(dtno$cut, col=2)

skew(dtno$cut) # es positiva, lo que significa que tiene cola hacia la derecha
kurtosis(dtno$cut) #
shapiro.test(dtno$cut)

# *** Conclucion variable CUT ***
# Lo que podemos concluir de estos graficos es la frecuencia de los cortes, teniendo el Ideal(#5) con mayor frecuencia y el Fair(#1) con menos.
# No existe una distribucion normal, confirmado con le shapiro test
# segun los datos del skew, tiene una cola hacia la derecha (confirmado en el histograma y densidad)

# *** Analisis variable COLOR ***
summary(dtno$color)

boxplot(dtno$color, horizontal = F)
hist(dtno$color)
# Se observa en el summary que la media y la mediana NO tienen valore similares, la preferencia esta distribuida aunque con mayor
# frecuencia en el color D(#4)

plot(density(dtno$color))
qqnorm(dtno$color)
qqline(dtno$color, col=2)

skew(dtno$color) # es positiva, lo que significa que tiene cola hacia la derecha
kurtosis(dtno$color) #
shapiro.test(dtno$color)

# *** Conclucion variable COLOR ***
# Al iguel que el anterior caso, la conclucion es la frecuencia de colores, apareciendoa mas el color D(#4) y el que menos el J(#7)
# No existe una distribucion normal, se hizo el contraste con el Shapiro test, con un valor <0.05
# Se confirma en el Skew, con una leve cola hacia el lado derecho

# *** Analisis variable CLARITY ***
summary(dtno$clarity)

boxplot(dtno$clarity, horizontal = F)
hist(dtno$clarity)
# Se observa en el summary que la media y la mediana muy cercannos, y el histograma nos confirma que la claridad SI1(#3) es la preferida 

plot(density(dtno$clarity))
qqnorm(dtno$clarity)
qqline(dtno$clarity, col=2)

skew(dtno$clarity) # es positiva, lo que significa que tiene cola hacia la derecha
kurtosis(dtno$clarity) #
shapiro.test(dtno$clarity)

# *** Conclucion variable CLARITY ***
# LOs valores que tienen mas frecuencia son el SI1 (numero 3) y el que menos I1 (numero 1), Por la forma del grafico 
# y analizando los valores Skew, Kurtosis y Shapiro y, se confirman que NO es una distribucion norma, y que tiene una cola hacia la derecha

# *** Analisis variable DEPTH ***
summary(dtno$depth)

boxplot(dtno$depth, horizontal = F)
hist(dtno$depth)
# Se observa en el summary que la media y la mediana similares, pareciendo tener una distribucion normal.

plot(density(dtno$depth))
qqnorm(dtno$depth)
qqline(dtno$depth, col=2)

skew(dtno$depth) # es positiva, lo que significa que tiene cola hacia la derecha
kurtosis(dtno$depth) #
shapiro.test(dtno$depth)

# *** Conclucion variable DEPTH ***
# se puede concluir que esta variable NO tiene una distribucion normal, Los valores de media y mediana son casi iguales
# El de skew nos dice que esta con una  cola a la izquierda y el kurtosis, nos indicaa que es puntiaguda
# el Test de shapiro nos confirma que NO tiene una distribucion normal

# *** Analisis variable TABLE ***
summary(dtno$table)

boxplot(dtno$table, horizontal = F)
hist(dtno$table)
# Se observa en el summary que la media y la mediana similares, pareciendo tener una distribucion normal.

plot(density(dtno$table))
qqnorm(dtno$table)
qqline(dtno$table, col=2)

skew(dtno$table) # es positiva, lo que significa que tiene cola hacia la derecha
kurtosis(dtno$table) #
shapiro.test(dtno$table)


# *** Conclucion variable TABLE ***
# Una vez analizados los valores y graficas, se puede ver que esta variable NO tiene una distribucion normal.
# Los valores de media y mediana son casi iguales, pero los graficos no tienen una distribucion normal.
# El test de skew da un valor distinto a cero con una cola a la derecha confirmandose en el kurtosis.
# el Resultado de shapiro, nos confirma que NO tiene una distribucion normal

# *** Analisis variable PRICE ***
summary(dtno$price)

boxplot(dtno$price, horizontal = F)
hist(dtno$price)
# a pesar de eliminacion de outliers, se visualizan, otros, por lo que se procede a una eliminacion manual. Crearemos un df para este caso.
dtnoT <- dtno[dtno$price<9000,]

dtno <- dtno[dtno$price<9000,]

# Se vuelvue a graficar
boxplot(dtnoT$price, horizontal = F) # ya no se visualizan  ourliers
hist(dtnoT$price)

plot(density(dtnoT$price))
qqnorm(dtnoT$price)
qqline(dtnoT$price, col=2)

skew(dtnoT$price) # es positiva, lo que significa que tiene cola hacia la derecha
kurtosis(dtnoT$price) #
shapiro.test(dtnoT$price)

# *** Conclucion variable PRICE ***
# Una vez analizados los valores y graficas, se puede ver que esta variable NO tiene una distribucion normal.
# Los valores de media y mediana son muy distintos y se debe a los precios se concentran en mayor media a bajo precio, se observa en los graficos
# El test de skew da un valor positivo por lo que esta con cola hacia la derecha,
# El Test de shapiro nos indica en su p-value, que NO sigue  una distribcion normal

# *** Analisis variable x ***
summary(dtno$x)

boxplot(dtno$x, horizontal = F)
hist(dtno$x)
# Se observa en el summary que la media y la mediana similares, y la fecuencia de X es muy dispersa

plot(density(dtno$x))
qqnorm(dtno$x)
qqline(dtno$x, col=2)

skew(dtno$x) # es positiva, lo que significa que tiene cola hacia la derecha
kurtosis(dtno$x) #
shapiro.test(dtno$x)

# *** Conclucion variable X ***
# Media y Mediana muy similares, sin picos o valores outliers, NO tieene una distribucion normal y los valoresde skew y kurtosis y shapiro
# confirman que NO es distribucion normal


# *** Analisis variable y ***
summary(dtno$y)

boxplot(dtno$y, horizontal = F)
hist(dtno$y)
# Se observa en el summary que la media y la mediana similares, y la fecuencia de X es muy dispersa

plot(density(dtno$y))
qqnorm(dtno$y)
qqline(dtno$y, col=2)

skew(dtno$y) # es positiva, lo que significa que tiene cola hacia la derecha
kurtosis(dtno$y)
shapiro.test(dtno$y)

# *** Conclucion variable y ***
# Media y Mediana muy similares, sin picos o valores outliers, NO tieene una distribucion normal y los valoresde skew y kurtosis y shapiro
# confirman que NO es distribucion normal

# *** Analisis variable z ***
summary(dtno$z)

boxplot(dtno$z, horizontal = F)
hist(dtno$z)
# Se observa en el summary que la media y la mediana similares, y la fecuencia de X es muy dispersa

plot(density(dtno$z))
qqnorm(dtno$z)
qqline(dtno$z, col=2)

skew(dtno$z) # es positiva, lo que significa que tiene cola hacia la derecha
kurtosis(dtno$z)
shapiro.test(dtno$z)

# *** Conclucion variable z ***
# Media y Mediana muy similares, sin picos o valores outliers, NO tieene una distribucion normal y los valoresde skew y kurtosis y shapiro
# confirman que NO es distribucion normal


# ******************************** Punto 3: Inferencia ********************************
# Calcula un intervalo de confianza para la media de "carat" y "depth"
# Formula un test de hipótesis

# Uso de la funcion t.test para una muestra, nos devuelve intervalo de confianza, como no se le indica otro parametro el default es 95%
# Intervalo de confianza para caract
t.test(dtno$carat)
intconfi <- intervalo <- t.test(dtno$carat)$conf.int[1:2]

par(mfrow=c(1,1))
plot(density(dtno$carat))
abline(v=mean(dtno$carat), col="red",lty=2, lwd=1)
abline(v=intconfi[1],col="blue",lty=1, lwd=1)
abline(v=intconfi[2],col="chocolate4",lty=1, lwd=1)
cat ("El intervalo de confianza, para carat es: [" , intconfi[1], " y ", intconfi[2], "] ")

# Intervalo de confianza para depth
t.test(dtno$depth)
intconfi <- intervalo <- t.test(dtno$depth)$conf.int[1:2]

plot(density(dtno$depth))
abline(v=mean(dtno$depth), col="red",lty=2, lwd=1)
abline(v=intconfi[1],col="blue",lty=1, lwd=1)
abline(v=intconfi[2],col="chocolate4",lty=1, lwd=1)
cat ("El intervalo de confianza, para depth es: [" , intconfi[1], " y ", intconfi[2], "] ")

# Formula un test de hipótesis
# La hipotesis nula (H0): la medida de precios de 2 cortes (Ideal y Premium) son iguales.
# La hiposiste alterna (H1): La media de precios de corte Iseal y Premium es distinta

Goodpr<-dtno$price[dtno$cut==2]
VeryGoodpr<-dtno$price[dtno$cut==3]

# Mostramos un histograma
par(mfrow=c(1,2))
boxplot(Goodpr)
boxplot(VeryGoodpr)

# Encontramos unos outliers, por lo que procedemos a eliminarlos
Goodpr<-Goodpr[which(Goodpr>1000)]
VeryGoodpr<-VeryGoodpr[which(VeryGoodpr>1000)]

# Graficamos nuevamente
par(mfrow=c(2,2))
boxplot(Goodpr)
hist(Goodpr)
boxplot(VeryGoodpr)
hist(VeryGoodpr)

t.test(Goodpr, VeryGoodpr,  alternative='two.sided')

# El resultado refleja que la media de los precios de los diamantes Good y VeryGood no es igual
# el p-value e muy pequeño, por lo que rechaza la hipotesis nula de que las medias de los precios serian iguales
# Se acepta la hipotesis alternativa


# ******************************** Punto 4: Relaciones entre las variables ********************************
# Muestra las relaciones que existen entre variables 
# (dependencia, anova, correlación)

# Recordando que las variables categoricas ya tienen valores numericos para mi data frame, no hay que hacer cambios.
par(mfrow=c(1,1))
head(dtno)


# Funcion para hacer el test de correlacion entre variables del dataset, se extrae del siguiente sitio web:
# http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

dtnocor <- cor(dtno)
# Pintamos grafico de correlaciones 
corrplot(dtnocor, method="number")
corrplot(dtnocor, type="upper")
p.mat <- cor.mtest(dtno)
head(p.mat[, 1:5])

# NUevamente hacermos un grafico de correlacion y marcamos los que tienen menos correlacion
corrplot(dtnocor, type="upper", order="hclust",  p.mat = p.mat, sig.level = 0.01)
cor.test(dtno$cut,dtno$table)

# Con varias pruebas, puedo concluir quien mas correlacion tienen son caract, con x, y, z (peso y medidas del diamante) y precio.

# *** ANOVA ***
# Generamos una muestra, Se igualan los daos para que la cantidad sea igual en todas estractos de Clarity
ResClarity <- count(dtno, vars = "clarity")
fmClarity <- min(ResClarity$freq)
muestraI1 <- sample(dtno[dtno$clarity==1,4], fmClarity,replace=FALSE)
muestraSI2 <- sample(dtno[dtno$clarity==2,4], fmClarity,replace=FALSE)
muestraSI1 <- sample(dtno[dtno$clarity==3,4], fmClarity,replace=FALSE)
muestraVS2 <- sample(dtno[dtno$clarity==4,4], fmClarity,replace=FALSE)
muestraVS1 <- sample(dtno[dtno$clarity==5,4], fmClarity,replace=FALSE)
muestraVVS2 <- sample(dtno[dtno$clarity==6,4], fmClarity,replace=FALSE)
muestraVVS1 <- sample(dtno[dtno$clarity==7,4], fmClarity,replace=FALSE)
muestraIF <- sample(dtno[dtno$clarity==8,4], fmClarity,replace=FALSE)

# creamos factores
dtClarity <- c(muestraI1,muestraSI2,muestraSI1,muestraVS2,muestraVS1,muestraVVS2,muestraVVS1,muestraIF)
factorClarity <- c(rep("I1",fmClarity),rep("SI2",fmClarity),rep("SI1",fmClarity),rep("VS2",fmClarity),rep("VS1",fmClarity),rep("VVS2",fmClarity),rep("VVS1",fmClarity),rep("IF",fmClarity))
factorClarity<-factor(factorClarity)

# Visalizamos datos para cada Clarity
split(dtClarity,factorClarity)

# Validamos Distribucion y varianza, requerido por ANOVA
shapiro.test(dtClarity)
d<-rnorm(length(dtClarity),mean=mean(dtClarity),sd=sd(dtClarity))
qqplot(dtClarity,d)
abline(c(0,1))


# Se realiza un contraste de independencia
reschi <- chisq.test(dtClarity, factorClarity, simulate.p.value=TRUE)
cat ("Rsultado de Reschi es: ")
print (reschi)
cat ("El resultado de pvalue :", reschi$p.value , " es > que 0.05 : " , reschi$p.value > 0.05)
# Si el p.value es mayor que 0.05, se concluye que son independientes. Para este caso despues de cambiar la muesta varias veces,
# se concluye que son Independientes



#muestra el valor del poder de transformacion 

splevel <- spreadLevelPlot(dtClarity, by=factorClarity, robust.line=FALSE, xlab="Nivel CO")
varpt <- round(splevel$PowerTransformation)
TrvdtClarity <- (dtClarity)**varpt # Transformacion devariable.

leveneTest(TrvdtClarity, group=factorClarity) # Mayor de 0.05, igualdad de varianzas
# EL Resultado indica que hay igualdad de  varianzas  despues de la transformacion), ya que da un resultado es mayor 0.05

# Despues de las validaciones y cumplidas las variables, se procede al calulo de ANOVA
p.aov<-aov(TrvdtClarity ~ factorClarity)  
summary(p.aov)

# Despues de ejecutar varias veces el ejercicio, 
# Elvalor de P es mayor que 0.05, se entiende que no hay diferencias significativas, Aceptando la hipotesis de que las medias sean iguales.

tukey<-TukeyHSD(p.aov) #los intervalos que no contienen el valor 0 son significativos, p-value<0,05
tukey
plot(tukey) #se puede ver en el grafico que no hay valores significativos.


# ******************************** Punto 5: Análisis de regresión ********************************
# Formular un modelo de regresión y analiza los resultados
# Muestra los residuos y analiza los resultados
# Aplica una transformación a la regresión y analiza los resultados
# Interpreta los coeficientes estandarizados de la regresión

# Formular un modelo de regresión y analiza los resultados
# 
#	Haremos un modelo de regresion precio(price) del diamante como variable dependiente de carat como variable independiente.
# Grafico de Relacion
pairs(dtno)

# grafico de distribucion de variables
par(mfrow=c(1,2))
boxplot(dtno$price, main = "Precio")
boxplot(dtno$carat, main = "Peso - Carat")

# coeficiente de correlacion
cor(dtno$price, dtno$carat)  #0.9328514
cor.test(dtno$price, dtno$carat) #p-value < 2.2e-16 , Ho: correlación nula

# Regresion Lineal
regreLineal <- lm(price ~ carat, data = dtno)
summary(regreLineal)

# El analisis del resultado nos indica que tenemos un valor residual de 798.2 con un 87% del comportamiento
# ademas los valores de Intercep y caract son significativos, mayores al 99.9% 
# El p-value es próximo a cero, menor de 0.05 por tanto se confirma que la pendiente es distinta de 0,
# lo que es lo mismo, el coeficiente de correlación poblacional es no nulo y el modelo es adecuado

# podemos confirmar con el anova, que exite una correlacion diferente de cero
anova(regreLineal)

# Mostramos graficamente como es la correlacion
par(mfrow=c(1,1))
plot(dtno$carat, dtno$price, xlab = "Peso - Carat", ylab = "Precio", main="Precio x Peso")
abline(regreLineal, col=2, lwd=3)

# Coeficientes
coef <- regreLineal$coefficients

# Y = B1 X + Bo
Y = coef[2]*dtno$price + coef[1]

# Muestra los residuos y analiza los resultados
#
# Residuos
summary(regreLineal$residuals)
sd(regreLineal$residuals)
boxplot(regreLineal$residuals)

plot(regreLineal$fitted.values,regreLineal$residuals)
abline(0,0)

# Prueba de Levene para igualdad de varianzas
grupo=ifelse(dtno$price<quantile(dtno$price,.25),1,
             ifelse(dtno$price>=quantile(dtno$price,.25) & dtno$price<quantile(dtno$price,.5),2,
                    ifelse(dtno$price>=quantile(dtno$price,.5) & dtno$price<quantile(dtno$price,.75),3,4 )      )     )

levene.test(regreLineal$residuals, group = grupo) #p-value 2.2e-16, NO se acepta la igualdad de varianzas, se ebe hacer una tranformacion

# Aplica una transformación a la regresión y analiza los resultados
#
spreadLevelPlot(regreLineal) 

##
priceT=sqrt(dtno$price)
rlt <- lm(priceT ~ dtno$carat)
summary(rlt)
plot(X,y, xlab = "Publicidad", ylab = "raíz cuadrada de Ventas", main="Gastos de publicidad x volumen de ventas")
abline(r)


