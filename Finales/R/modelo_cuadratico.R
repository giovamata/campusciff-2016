library(ggplot2)

diamonds_ideal <- diamonds[diamonds$cut == "Ideal", ]

# modelo cuadratico
fit <- lm(price ~ carat + I(carat^2), data=diamonds_ideal)
plot(price ~ carat, diamonds_ideal, pch=20)

# ordenamos los valores, de lo contrario se pintan los puntos desordenados y, al unirlos 
# con lineas crean ese efecto "zigzag"
idx <- order(diamonds_ideal$carat)
x_val <- diamonds_ideal$carat[idx]
y_val <- predict(fit)[idx]

# pintamos la linea del ajuste
lines(x_val, y_val, col="red", lwd=3)

# otra forma, muy parecido a como pintábamos los valores de la función dnorm:

# generamos secuencia ficticia de valores de "carat" (eje x)
x_val <- seq(min(diamonds_ideal$carat), max(diamonds_ideal$carat), by = 0.01)

# calculamos el valor del modelo (función cuadrática) para esos valores
y_val <- predict(fit, data.frame(carat=x_val))

# pintamos la linea
lines(x_val, y_val, col="blue", lwd=3)