# main.R

# Cargar las funciones desde functions.R
source("func.R")

# Llamar a la función calcular_promedio
resultado <- calcular_promedio(mtcars)

# Imprimir el resultado
cat("El promedio de hp es:", resultado, "\n")
