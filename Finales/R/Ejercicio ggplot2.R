#Se carga la libreria ggplot2
library(ggplot2)

#estructura de diamonds, observando algunas lineas de dataset
tail(diamonds)

#tomamons una muestra de 100

dreducido <- diamonds[sample(nrow(diamonds), 100), ]

#Graficamos
p <- ggplot(dreducido, aes(carat, price, color=color)) + geom_point()
p + geom_smooth(method = 'lm', formula=y~x, se = FALSE, aes(group=1))
