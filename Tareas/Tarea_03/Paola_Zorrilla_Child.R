

library(feather)
library(tidyverse)

#casen = read_feather("C:/Users/pzorrilla/Desktop/prueba/casen_2020_edit.feather")


# Ejercicio 1----

#Selecionar el Modulo de ocupación

Modulo_ocupacion <- casen %>%
  select(matches("^o\\d+|^of|^rama"))


#Selecionar el Modulo de vivienda

Modulo_vivienda <- casen %>%
  select(matches("^v\\d+"))


#Ejercicio 2----


library(tm)

# Procesar Texto

pro_tex <- function(texto) {
  texto <- tolower(texto) #
  texto <- str_replace_all(texto, "[[:punct:]]", "") 
  texto <- str_replace_all(texto, "[[:digit:]]", "")
  stopwords <- stopwords("es")
  texto <- removeWords(texto, stopwords)
  texto <- str_replace_all(texto, "\\s+", " ")
  texto <- trimws(texto)
  return(texto)
}


# Ejemplo de uso
prueba <- "1. Lo siguiente, es para mostrar el uso de la funcion, en 5,4,3,2,1"
(texto_procesado <- pro_tex(prueba))












