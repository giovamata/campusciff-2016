# Proyecto Final

# Proyecto I pagina 26

library(ggplot2)
library(dplyr)
library(plyr)
library(outliers)

# Cargar los datos diabetes.data en R
diabetestotal <- read.csv("Master/TrabajosFinales/R/diabetes.data", header = TRUE, sep = "\t")

# Eliminar los missing values, que estan codicados como -9999.00
diabetes <- read.csv("Master/TrabajosFinales/R/diabetes.data", header = TRUE, sep = "\t", na.string="-9999.0")
if (sum(is.na(diabetes)) > 0)
  diabetes <-na.omit(diabetes)

# Ver el tipo de cada una de las variables.
str(diabetes)
#'data.frame':	433 obs. of  11 variables:
#$ AGE: int  
#$ SEX: Factor w/ 2 levels "F","M": 
#$ BMI: num 
#$ BP : num 
#$ S1 : int 
#$ S2 : num 
#$ S3 : num 
#$ S4 : num
#$ S5 : num
#$ S6 : int
#$ Y  : int

# Realizar un analisis estadstico de las variables: calcular la media, varianza, rangos, etc.
# Tienen las distintas variables rangos muy diferentes?.
dim(diabetes)
apply(diabetes[-2], 2, mean)
apply(diabetes[-2], 2, range)
apply(diabetes[-2], 2, quantile, probs=c(0.25, 0.50, 0.75))
apply(diabetes[-2], 2, sd)

# Respuesta: despues del resultado, eliminado la columna SEX, se observa valores distintos o dispersos

# Hacer un grafico de cajas (boxplot) doonde se pueda ver la informacioon anterior de forma grafica.

boxplot(diabetes[-2])

# Calcular la media para las filas que tienen SEX=M y la media para las filas que tienen SEX=F, utilizando la funcion tapply
diabetesmedia = diabetes[-2]
for (i in 1:ncol(diabetesmedia)) {
  print(colnames(diabetesmedia[i]))
  print(tapply(diabetesmedia[[i]], diabetes$SEX, mean))
}

# Proyecto II pagina 27
# Calcular la correlacion de todas las variables nummericas con la variable Y

# creado matriz de corelacion 
cor_diabetes<-cor(diabetes[-2])[,ncol(diabetes[-2])]

# muestra matriz de correlacion por pantalla
cor_diabetes

#  Realizar un grafico de dispersion para las variables que tienen mas y menos correlacion con Y y comentar los resultados. 
# Como seria el grafico de dispersion entre dos cosas con correlacion 1?

ggplot(diabetes,aes(x=diabetes$BMI, y=diabetes$Y))+geom_point(aes(colour = "red"))

ggplot(diabetes,aes(x=diabetes$S2, y=diabetes$Y))+geom_point(aes(colour = "red"))

# COmentarios: La maxima se observa con BMI y la minima con S2.
# Como seria el grafico de dispersion entre dos cosas con correlacion 1? Respuesta: Se obtiene una linea de 45 grados
ggplot(diabetes,aes(x=diabetes$Y, y=diabetes$Y))+geom_line()

# Transformar la variable SEX, que es un factor, en una variable numerica utilizando, por ejemplo, la codificacion M=1 y F=2.
# Existe una forma sencilla de hacerlo, aunque los valores quedarian asi: F=1 y M=2, utilizare un data frame adicional
diabetes_adicional <- diabetes
diabetes_adicional$SEX <- as.numeric(diabetes_adicional$SEX)
tail(diabetes_adicional)

# COmo queremos que los valores queden M=1 y F=2, agregaremos y eliminaremos columnas
diabetes_Sexinteger = mutate(diabetes, SEXinteger=ifelse(SEX=="M", 1,2))
diabetes_Sexinteger = diabetes_Sexinteger %>% select(AGE,SEXinteger, BMI, BP, S1, S2, S3, S4, S5, S6, Y)

# DeFInimos los outliers como los elementos (FIlas) de los datos para los que cualquiera de las variables esta por
# encima o por debajo de la mediana mas/menos 3 veces el MAD (Median Absolute Deviation). Identificar estos y quitarlos

# define a function to remove outliers (buscando ayuda en los foros, reusando codigo de Internet y adaptando para media y mediana )
# gracias a http://rpubs.com/lcollado/7904

findOutlier <- function(data, cutoff = 3) {
  ## Calculando la Media y Mediana
  mediana <- apply(data, 2, median, na.rm = TRUE)
  datomad <- apply(data, 2, mad, na.rm = TRUE)
  ## Identificando las celdas con valor mayor al cutoff = 3 
  result <- mapply(function(da, me, ma) {
    which(da > me+cutoff*ma|da < me-cutoff*ma)
  }, data, mediana, datomad)
  result
}

outliers <- findOutlier(diabetes[-2])
outliers 

removeOutlier <- function(data, outliers) {
  result <- mapply(function(d, o) {
    res <- d
    res[o] <- NA
    return(res)
  }, data, outliers)
  return(as.data.frame(result))
}

Diabetes_sinoutliers <- removeOutlier(diabetes[-2], outliers)
Diabetes_sinoutliers <-na.omit(Diabetes_sinoutliers)
nrow(diabetes)
nrow(Diabetes_sinoutliers)
Diabetes_sinoutliers

# Separar el conjunto de datos en dos, el primero (entrenamiento) conteniendo un 70% de los datos y el
# segundo (test) un 30%, de forma aleatoria.

dfsplit= Diabetes_sinoutliers
str(dfsplit)

corte <- floor(nrow(dfsplit)*0.70)         #define % of training and test set

set.seed(9)
aleatorios <- sample(seq_len(nrow(dfsplit)), size = corte)
str(aleatorios)

# Generaion de DF entranamiento
dfsplit_train <- dfsplit[aleatorios, ]
str(dfsplit_train)

# Generaion de DF test
dfsplit_test <- setdiff(dfsplit,dfsplit_train)
str(dfsplit_test)

# Escalar los datos para que tengan media 0 y varianza 1, es decir, restar a cada variable numerica su media y dividir 
# por la desviacion tipica. Calcular la media y desviacion en el conjunto de train, y utilizar esa misma media y
# desviacion para escalar el conjunto de test.
head(dfsplit_test)
head(dfsplit_train)

valmedia <- numcolwise(mean)(dfsplit_train)
valdesviacion <- numcolwise(sd)(dfsplit_train)

for(i in 1:nrow(dfsplit_train)){
  
  dfsplit_train[i,c(1:ncol(dfsplit_train))] <- (dfsplit_train[i,c(1:ncol(dfsplit_train))] - valmedia) / valdesviacion
  
}

for(i in 1:nrow(dfsplit_test)){
  
  dfsplit_test[i,c(1:ncol(dfsplit_test))] <- (dfsplit_test[i,c(1:ncol(dfsplit_test))] - valmedia) / valdesviacion
  
}

# Realizar un modelo de regresion lineal de la variable de respuesta sobre el resto y ajustarlo por minimos
# cuadrados usando unicamente los datos del conjunto de entrenamiento.

regre_dftrin <- lm(Y ~ AGE + BMI + BP + S1 + S2 + S3 + S4 + S5 + S6, data=dfsplit_train)
summary(regre_dftrin)


# Calcular el error cuadratico medio de los datos del conjunto de entrenamiento y de los datos del conjunto
# de test, de nido como (formula en pdf) donde y es el vector de respuesta de los datos y ^y es el
# vector que predice el modelo (para los mismos datos).


vpredict <- predict(regre_dftrin)
ERRcuamed <- mean((dfsplit_train$Y - vpredict)^2)
cat("\n Respuesta ECM:", ERRcuamed, "\n")


epredict <- predict(regre_dftrin, dfsplit_test)
ERRcua <- mean((dfsplit_test$Y - epredict)^2)
cat("\n Respuesta:", ERRcua, "\n")