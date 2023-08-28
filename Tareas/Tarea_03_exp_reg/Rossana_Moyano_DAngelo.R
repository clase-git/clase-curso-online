# Ejercicio 1

# A partir de la base de datos de la encuesta Casen 2020, disponible en la carpeta de la clase de hoy "casen_2020_edit.feather", 
# realice las siguientes tareas
# 
library(feather)
library(tidyverse)
casen = read_feather("C:/Users/PERSONAL/Desktop/Curso R Intermedio/casen_2020_edit.feather")

# 1- Ayudándote de expresiones regulares, y de la manera más suscinta que puedas, selecciona todas las variables de cuestionario de:
#   
# - el módulo de Ocupación
# - el módulo de vivienda
# 
# El output debe ser una tabla solo con las variables de cada módulo, por separado.
# nota: puedes omitir las variables construidas ex-post en cada módulo.


# Resolución ejercicio 
library(dplyr)

# Con expresiones regulares
tabla_modulo_ocupados <- casen[, grep("^o", names(casen))]
tabla_modulo_vivienda <- casen[, grep("^v", names(casen))]

# Otra forma de hacerlo
tabla_modulo_ocupados <- casen %>% select(starts_with("o"))
tabla_modulo_vivienda <- casen %>% select(starts_with("v"))

# Imprimir las tablas resultantes
print(tabla_modulo_ocupados)
print(tabla_modulo_vivienda)

# Ejercicio 2 
# 
# La tabla "casen_2020_edit.feather" contiene las variables o9a, o9b y o24, que representan, respectivamente, "ocupación", 
# "tareas en la ocupación" y "rama de actividad económica" de las personas que declaran estar ocupadas en el periodo de referencia.
# 
# Estas variables son utilizadas como insumo para la codificación automática de los clasificadores de ocupación y rama de actividad económica.
# 
# Crea una función que reciba como argumento una variable character y la procese de la siguiente manera:
#   
# - Pase todos los caracteres de una glosa a minúscula
# - Remueva todos los signos de puntuación y caracteres especiales
# - Remueva todos los números
# - Extraiga espacios adicionales entre palabras
# - Remueva *stopwords* (para esto pueden usar las librerías `tm`, `quanteda`, entre otras)
# 
# La función debe retornar una variable con glosas procesadas.


# Resolución ejercicio
library(tm)  # Para remover stopwords

procesar_glosa <- function(glosa) {
  glosa <- tolower(glosa)  # Convertir a minúsculas
  glosa <- gsub("[[:punct:]]", "", glosa)# Remover signos de puntuación y caracteres especiales
  glosa <- removeWords(glosa, stopwords("spanish"))  # Remover stopwords
  glosa <- gsub("[[:digit:]]", "", glosa) # Remover números
  glosa <- gsub("\\s+", " ", glosa)  # Extraer espacios adicionales entre palabras
  return(glosa)
}

# Ejemplo de uso
glosa_ejemplo <- "Esta 234 es una antes glosa con --<números 123 y signos de puntuación!"
glosa_procesada <- procesar_glosa(glosa_ejemplo)
print(glosa_procesada)

tarea_ocupacion <- casen$o9a[92]
tarea_procesada <- procesar_glosa(tarea_ocupacion)
print(tarea_procesada)