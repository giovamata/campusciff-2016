## ASIGNATURA: Analisis Estadistico
## PROFESOR: Antonio Pita Lozano
## ALUMNO: Edwin Giovanni Gonzalez Mata
## Segundo Ejercicio: Prediccion de Series Temporales
## -------------------------------------------------------------------------

library(forecast)

setwd("C:/Users/Giovanni/Documents/Master/TrabajosFinales/Analisis Estadistico/Practica 2")
 
## Carga de datos a Dataset
MilkProd=read.csv("monthly-milk-production-pounds-p.csv",head=TRUE,sep=";")
head(MilkProd)

## Cambiando nombre de las columnas 
colnames(MilkProd) <- c("Date","Production")
head(MilkProd)

## Analisis de datos del dataset
str(MilkProd)
summary(MilkProd)

## Se observan nulos, se procede a elilminarlos 
MilkProd <- na.omit(MilkProd)
summary(MilkProd)

## Ploteando Dataset
plot(MilkProd)

## Convertimos datos a series temporales, para poder analisar datos
MilkProdTS=ts(MilkProd$Production,frequency=12,start=1962)

## Se hace un PLOT y se observan tendendias crecientes con el tiempo
plot(MilkProdTS)
abline(reg=lm(MilkProdTS~time(MilkProdTS)))

## HAcemos un PLOT sobre un periodo de tiempo
par(mfrow=c(1,2))
acf(MilkProdTS,lag.max=48) 
pacf(MilkProdTS,lag.max=48) 

## Vemos una tendencia clara en relacion al tiempo (anual), no es una situacion estacionaria.
par(mfrow=c(1,1))
tsdisplay(MilkProdTS)

## Para este caso, podemos utilizar el modelo ARIMA para prediccion.

MilkProdTS_Model=auto.arima(MilkProdTS,trace=TRUE) # esto lo hace todo
summary(MilkProdTS_Model)
## R/  Best model: ARIMA(0,1,1)(0,1,1)[12]

## Visualizamos residuos de la prediccion.
tsdisplay(residuals(MilkProdTS_Model))
par(mfrow=c(1,2))
hist(residuals(MilkProdTS_Model))
qqnorm(residuals(MilkProdTS_Model)); qqline(residuals(MilkProdTS_Model),col=2)

## Hacemos un Plot de la Prediccion y mostramos la prediccion
par(mfrow=c(1,1))
MilkProd_FC = forecast(MilkProdTS_Model,h=24)
plot(MilkProd_FC)
MilkProd_FC

Result=arima(MilkProdTS,order=c(0,1,1),seasonal=list(order = c(0, 1, 1)))
plot(forecast(Result,h=24))
grid()
summary(Result)

