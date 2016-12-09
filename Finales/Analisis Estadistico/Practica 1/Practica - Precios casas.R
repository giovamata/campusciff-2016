## ASIGNATURA: Analisis Estadistico
## PROFESOR: Antonio Pita Lozano
## ALUMNO: Edwin Giovanni Gonzalez Mata
## Primer Ejercicio: Construcción de un modelo de regresión.
## -------------------------------------------------------------------------
##Introducción: Habéis sido contratados para realizar un estudio sobre el precio de las viviendas en estados unidos. 
##              El proyecto tiene dos objetivos:
##                  1.- Analizar el efecto de la superficie de la vivienda en el precio de la vivienda
##                  2.- Estimar el precio de venta de unos inmuebles de la cartera de la empresa.
## -------------------------------------------------------------------------
##
## Los datos con que trabajaremos y que contiene el archivo son:
##
## id: identificador de la vivienda
## date: fecha de la venta de la vivienda
## price: precio de la vivienda
## bedrooms: número de habitaciones
## bathrooms: número de baños
## sqft_living: superficie de la vivienda (en pies) 
## sqft_lot: superficie de la parcela (en pies)
## floors: número de plantas
## waterfront: indicador de estancia en primera línea al mar
## view: número de orientaciones de la vivienda
## condition: estado de la vivienda (mayor es mejor)
## grade: calidad de la construcción (mayor es mejor)
## sqft_basement: superficie de la planta baja (en pies)
## sqft_above: sqft_living - sqft_basement
## yr_built: año de construcción
## yr_renovated: año de reforma
## zipcode: codigo postal
## lat: latitud
## long: longitud
## sqft_living15: superficie media de las 15 viviendas más cercanas
## sqft_lot15: superficie media de la parcela de las 15 viviendas más cercanas
##
##
##
## PARTE 1: Analizar el efecto de la superficie de la vivienda en el precio de la vivienda
## ----------------------------------------------------------------------------------------
## Directorio de Trabajo y LIbrerias

library(MASS)
library(caTools)

setwd("C:/Users/Giovanni/Documents/Master/TrabajosFinales/Analisis Estadistico/Practica 1")

## -----------------------------------------------------------------------------------------
## Cargamos datos de entrenamiento, vemos sus datos, analizamos y formateamos variables

house_train=read.csv("house_train.csv")
str(house_train)
head(house_train)
summary(house_train)

house_train$date= as.Date(substr(house_train$date, 0, 8), "%Y%m%d")

## Graficamos price y Superficie de la vivienda 
par(mfrow=c(1,2))
    
## Analizamos la distribución del precio de las viviendas, que n parecen distribucion Normal
hist(house_train$price)
## Aplicamos un log y parece que ahora si genera una distribucion normal
hist(log(house_train$price))

## Tenemos 2 superficies (habitable y del terreno), lo qu se puede ver en las graficas siguiente es
## que existe relacion entre el precio y superficie habitable y NO existe con la superficie del terreno.
sqft_living = as.integer(house_train$sqft_living)
precio = as.numeric(house_train$price)
sqft_lot = as.integer(house_train$sqft_lot)

plot(house_train$price,house_train$sqft_living, xlab="Precio de Casa",ylab="Superficie habitable")
abline((lm(sqft_living ~ precio)), col='red') 
plot(house_train$price,house_train$sqft_lot, , xlab="Precio de Casa",ylab="Superficie de Terreno")
abline((lm(sqft_lot ~ precio)), col='red') 

## Modelos de Regresion (En funcion de la superficie habitable) 
## Correlación entre el precio y la superficie de la vivienda
cor(house_train$price, house_train$sqft_living)
modelo1=lm(price~sqft_living, data=house_train)
summary(modelo1)

## EL resultado es un Adjusted R-squared:  0.4941 y un sqft_living    281.959
## Ahora vemos los residuos
par(mfrow=c(2,2))
plot(modelo1$residuals)
smoothScatter(modelo1$residuals)
hist(modelo1$residuals)
qqnorm(modelo1$residuals); qqline(modelo1$residuals,col=2)
confint(modelo1, level=0.95)

## Los residuos se concentran alrededor del cero, con variacion en los extremos,
## Aunque el modelo se mira bueno, probaremos otros modelos para verificar si mejora el mismo.
## Transformaremos el precio.

modelo2=lm(log(price)~sqft_living,data=house_train)
summary(modelo2)

## EL resultado es un Adjusted R-squared:  0.4828

## Analizamos los residuos
plot(modelo2$residuals)
smoothScatter(modelo2$residuals)
hist(modelo2$residuals)
qqnorm(modelo2$residuals); qqline(modelo2$residuals,col=2)
confint(modelo2, level = 0.95)

## El resultado y comparativo de los modelos anteriore es: Los residuos siguen una distribucion normal cercana a cero.
## EL qqplot se ajusta mejor que la anterior (sin transformacion o sea, sin semielasticidad) aunque en los extremos
## se ve un poco de variacion. El Adjusted R-squared es muy parecido en los modelos anteriores.

## Haremos una prueba con un metodo estadístico robusto para analizar el resultado. 
modelo3=rlm(log(price)~sqft_living,data=house_train)
summary(modelo3)

## EL resultado es: Residual standard error: 0.4056 on 17382 degrees of freedom

## Analizamos los residuos
plot(modelo3$residuals)
smoothScatter(modelo3$residuals)
hist(modelo3$residuals)
qqnorm(modelo3$residuals); qqline(modelo3$residuals,col=2)

## Analizando los resultado, se observa que se comporta muy similar al modelo simple, sin muchs variaciones

## CONCCLUSION: 
## Vemos que el precio de la vivienda tiene una fuerte relación con la superficie habitable 
## Despues de aplicado y analizado los modelos, el que mejor se ajusta es el de semielasticidad. y los datos que 
## obtenemos es (con un 95%) de confianza: 
##                                   2.5 %         97.5 %
##              (Intercept)   1.220601e+01   1.223382e+01
##              sqft_living   3.909856e-04   4.032067e-04


## PARTE 2: Estimar el precio de venta de unos inmuebles de la cartera de la empresa
## ---------------------------------------------------------------------------------
## Cargamos el conjunto para el TEST
house_test = read.csv("house_test.csv")
house_test$date= as.Date(substr(house_test$date, 0, 8), "%Y%m%d")

sample = sample.split(house_train$price, SplitRatio = 0.8)
house_train_TR = subset(house_train, sample == TRUE)
house_train_VA = subset(house_train, sample == FALSE)

## Efectuamos una matriz de correlacion con las variables continuas
var_cont = c("price", "yr_renovated", "yr_built", "sqft_living", "sqft_lot", "sqft_above", "sqft_basement", "sqft_living15", "sqft_lot15")
house_train_TR_COR <- subset(x = house_train_TR, select = var_cont)
cor(house_train_TR_COR)[1,]

## Las variables que  mas aportan al precio son:
## sqft_living = 0.70800366
## sqft_above = 0.61124938
## sqft_living15 = 0.58177204
## sqft_basement = 0.33780664

training_data<-house_train_TR
validation_data<-house_train_VA
test_data<-	house_test
## Vamos crear un Modelo de Regresion Lineal con todas las variables
## Creating a linear model
linearModel <- lm(formula = price ~., data = training_data)
summary(linearModel)

## Tenemos un Adjusted R-squared:  0.7075 

plot(linearModel$residuals)
smoothScatter(linearModel$residuals)
hist(linearModel$residuals)
qqnorm(linearModel$residuals); qqline(linearModel$residuals,col=2)
confint(linearModel,level=0.95)

## El modelo parece ajustado, aunque con desviasiones al final

## Modificamos el MOdelo Linear y eliminamos varibales que no aportan 
linearModel_2=lm(log(price)~sqft_living+sqft_living15+view+waterfront+grade+date+zipcode,data=training_data)
summary(linearModel_2)

## Resultado Adjusted R-squared:  0.5999

plot(linearModel_2$residuals)
smoothScatter(linearModel_2$residuals)
hist(linearModel_2$residuals)
qqnorm(linearModel_2$residuals); qqline(linearModel_2$residuals,col=2)
confint(linearModel_2,level=0.95)
## Ya no se observan muchas variaciones en el Q-Q Plot

## Transformamos variables y aplicamos nuevamente un modelo Linear
training_data$view=as.factor(training_data$view)
training_data$waterfront=as.factor(training_data$waterfront)
training_data$grade=as.factor(training_data$grade)
training_data$zipcode=as.factor(training_data$zipcode)

# Modificamos el MOdelo Linear y eliminamos varibales que no aportan 
linearModel_3=lm(log(price)~sqft_living+sqft_living15+view+waterfront+grade+date+zipcode,data=training_data)
summary(linearModel_3)

## Resultado  R-squared:  0.8724 

plot(linearModel_3$residuals)
smoothScatter(linearModel_3$residuals)
hist(linearModel_3$residuals)
qqnorm(linearModel_3$residuals); qqline(linearModel_3$residuals,col=2)
confint(linearModel_3,level=0.95)

## Evaluamos los modelos

AIC(linearModel)
AIC(linearModel_2)
AIC(linearModel_3)

## Ahora, vamos a validar el Modelo
## Transformacion de los datos de validación
validation_data$view=as.factor(validation_data$view)
validation_data$waterfront=as.factor(validation_data$waterfront)
validation_data$grade=as.factor(validation_data$grade)
validation_data$zipcode=as.factor(validation_data$zipcode)

training_data$prediction=predict(linearModel_3,type="response")
R2_Train=1-sum((training_data$price-training_data$prediction)^2)/sum((training_data$price-mean(training_data$price))^2)

validation_data$prediction=predict(linearModel_3,newdata=validation_data,type="response")
R2_Test=1-sum((validation_data$price-validation_data$prediction)^2)/sum((validation_data$price-mean(validation_data$price))^2)

R2_Train
R2_Test
## El resultado de los errores es muy cercano, el modelo es aceptable

## Aplicacion del modelo al conjunto de TEST

## Transformacion de variables 
test_data$view=as.factor(test_data$view)
test_data$waterfront=as.factor(test_data$waterfront)
test_data$grade=as.factor(test_data$grade)
test_data$zipcode=as.factor(test_data$zipcode)

# Aplicamos el modelo que hemos seleccionado previamente
test_data$prediction=predict(linearModel_3,newdata=test_data,type="response")

# Al haber utilizado la transformación de semielasticidad, tenemos que convertir el precio a valores
# normales.
test_data$prediction = exp(test_data$prediction)

# Observamos los valores del dataset final
str(test_data)
head(test_data)
summary(test_data)

par(mfrow=c(1,2))
hist(test_data$prediction)
hist(log(test_data$prediction))

write.csv(test_data, file = "house_prediction.csv")
