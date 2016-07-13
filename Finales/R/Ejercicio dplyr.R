#Dataset diamon
library(dplyr)
library(ggplot2)

# Filtrar los diamantes con corte \Ideal".
filter(diamonds, cut=="Ideal")

# Seleccionar las columnas carat, cut, color, price y clarity
select(diamonds, carat, cut, color, price, clarity)

# Crear una nueva columna precio/quilate
mutate(diamonds, PrecioKilate = price / carat)

# Si queremos que se quede grabado en un nuevo dataset
diamondsNueva <- mutate(diamonds, PrecioKilate = price / carat)
diamondsNueva

# Agrupar los diamantes por color, adicional obtengo el precio promedio
summarise(group_by(diamondsNueva, color), PrecioPromedio= mean(PrecioKilate, na.rm = TRUE))


#Ordenar por precio/quilate de forma descendente.
arrange(diamondsNueva, desc(PrecioKilate))