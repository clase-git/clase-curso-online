library(feather)
library(tidyverse)
#setwd("C:/Users/psalazar/Documents/Documents/paulina/GIT")
# Cargar los datos desde el archivo feather
#casen <- read_feather("casen_2020_edit.feather")

# Seleccionar variables del módulo de Ocupación
casen_ocupacion <- casen %>%
  select(matches("^o\\d+|^of|^rama"))

# Seleccionar variables del módulo de Vivienda
casen_vivienda <- casen %>%
  select(matches("^v\\d+"))

# Mostrar las primeras filas de las tablas resultantes
head(casen_ocupacion)
head(casen_vivienda)




library(tm)
library(stringr)

procesar_glosa <- function(glosa) {
  # Pasar a minúsculas
  glosa <- tolower(glosa)
  
  # Remover signos de puntuación y caracteres especiales
  glosa <- str_replace_all(glosa, "[[:punct:]]", " ")
  
  # Remover números
  glosa <- str_replace_all(glosa, "[[:digit:]]", "")
  
  # Extraer espacios adicionales
  glosa <- str_replace_all(glosa, "\\s+", " ")
  
  # Remover stopwords (lista de ejemplo)
  stopwords <- c("esta", "es", "una", "de", "con", "algunos", "como")
  
  # Convertir la glosa en un vector de palabras
  palabras <- unlist(str_split(glosa, " "))
  
  # Filtrar las palabras según las stopwords
  palabras_filtradas <- palabras[!(palabras %in% stopwords)]
  
  # Reconstruir la glosa procesada
  glosa_procesada <- str_c(palabras_filtradas, collapse = " ")
  
  return(glosa_procesada)
}

# Ejemplo de uso
glosa_original <- "Esta es una glosa de ejemplo, con algunos números como 12345."
glosa_procesada <- procesar_glosa(glosa_original)
print(glosa_procesada)