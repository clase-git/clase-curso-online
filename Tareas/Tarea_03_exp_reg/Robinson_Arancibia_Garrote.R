#Ejercicio1
casen<- read_feather("C:/Users/Robinson/Downloads/casen_2020_edit.feather")

# Seleccionar variables del módulo de Ocupación
var_ocupacion <- casen %>% 
                       select(matches("ocup.*"))

# Seleccionar variables del módulo de Vivienda
var_vivienda <- casen %>% 
                      select(matches("viv.*"))

# Mostrar tabla de variables de Ocupación
View(var_ocupacion)

# Mostrar tabla de variables de Vivienda
View(var_vivienda)


#Ejercicio 2

procesar_variable <- function(variable) {
# Pasar a minúscula
variable <- tolower(variable)
  
# Remover signos de puntuación y caracteres especiales
variable <- gsub("[[:punct:]]", "", variable)
  
# Remover números
variable <- gsub("[[:digit:]]", "", variable)
  
# Extraer espacios adicionales entre palabras
variable <- gsub("\\s+", " ", variable)
  
# Remover stopwords utilizando la librería 'tm'
stop_words <- stopwords("es")
variable <- removeWords(variable, stop_words)
  
return(variable)
}

# Ejemplo de uso de la función para una variable 
variable_ejemplo <- "probemos este ejemplo 3131 con signos <- y números!"
ejemplo_1 <- procesar_variable(variable_ejemplo)
print(ejemplo_1)

tarea_ocupacion <- casen$o9a
tarea_procesada <- procesar_variable(tarea_ocupacion)
print(tarea_procesada)

