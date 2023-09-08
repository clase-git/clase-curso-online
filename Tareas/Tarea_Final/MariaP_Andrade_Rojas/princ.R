# main.R

# Cargar las funciones desde functions.R
source("func.R")

# Llamar a la función calcular_promedio
resultado <- calcular_promedio(mtcars$hp)

# Imprimir el resultado
cat("El promedio de ingresos es:", resultado, "\n")
