##TAREA 3
library(feather)
library(tidyr)
library(stringr)
library(tm)
#setwd("C:/Users/pc_personal/Desktop/GITTT/")
casen<-read_feather("casen_2020_edit.feather")

# Ejerciocio 1 

# Módulo de ocupación

tibble(m_ocupacion = str_subset(names(casen), pattern = "^o\\d"))

# Módulo de vivienda

tibble(m_vivienda = str_subset(names(casen), pattern = "^v\\d"))



# Ejercicio 2


mod_text <- function(variable) {
  variable <- tolower(variable) # pasar  texto a minusculas
  variable <- str_replace_all(variable, "[[:punct:]]", "") #remover signos de puntuación
  variable <- str_replace_all(variable, "[[:digit:]]", "") # remover digitos
  variable <- str_replace_all(variable, "\\s+", " ") ## remover espacios
  variable <- removeWords(variable, stopwords("es"))## Quitar palabras sin singnificado
  
  return(variable)
}

#mod_text(casen$o9a)
