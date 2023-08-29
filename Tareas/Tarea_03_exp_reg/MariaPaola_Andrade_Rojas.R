
###### TAREA 3 ###### MARIA PAOLA ANDRADE ROJAS ######

#######################
##### EJERCICIO 1 #####
#######################

library(feather)
library(dplyr)

#1. Ruta absoluta para trabajo con computador personal (bbdd enviada por Teams):

#setwd("C:/Users/ASUS Zenbook 14/Desktop/INE/Curso R INE 2023/Tarea 3")  

#2. Conversión de bbdd a dataframe para facilitar detección de patrones solicitados

casen2020 <- read_feather("casen_2020_edit.feather")
casen2020_df <- as.data.frame(casen2020)

#3. Para visualizar y corroborar variables a seleccionar

str(casen2020_df, list.len=ncol(casen2020_df))

#4. Selección de módulos, columnas contienen "ocupación" y "vivienda".
# Referencias: "Cuestionario Casen Pandemia 2020" y "Libro de Códigos Casen 2020".

var_ocup_viv <- "^(o[0-9]|o[0-9]{2}|o[0-9][a-z]|o[0-9]{2}[a-z]|o[0-9]_[a-z]{3}|o[0-9]{2}_[a-z]{3}|activ|activ[0-9]|rama[0-9]|rama[0-9]_rev[0-9]|ocup_inf|oficio[0-9]_88|oficio[0-9]_08|v[0-9]|v[0-9]{2}|v[0-9]_casa|v[0-9]_depto|v[0-9]{2}_propia|v[0-9]{2}_esp|v[0-9]{2}_arrendada|v[0-9]{2}_cedida|v[0-9]{2}_preg|v[0-9]{2}_cajon|v[0-9]{2}_red|v[0-9]_sistema|v[0-9]{2}_sistema)$"

tablamod_ocup_viv <- casen2020_df %>%
  select(matches(var_ocup_viv))

names(tablamod_ocup_viv)

#######################
##### EJERCICIO 2 #####
#######################

library(tm)
library(quanteda)

## Crear función que reciba argumento variable character y la procese según condiciones dadas ##

procesar_glosa <- function(glosa) {
  
  glosa <- tolower(glosa) #Convertir todo a minúscula
  
  glosa <- gsub("[[:punct:]]", "", glosa) #Remover puntuación y caracteres especiales
  
  glosa <- gsub("[[:digit:]]", "", glosa) #Remover números
  
  glosa <- gsub("\\s+", " ", glosa) #Extraer espacios adicionales
  
  tokens <- tokens(glosa) #Partición del texto para facilitar proceso 
  
  stopwords <- stopwords("es") #Español para no obtener errores, no encontré alternativa genérica
  
  tokens <- tokens_remove(tokens, stopwords) #Remover stopwords (palabras sin significado)
  
  glosa_procesada <- as.character(tokens)
  
  return(glosa_procesada)
}

### Variables solicitadas a procesar ###

o9a <- casen2020_df$o9a
o9b <- casen2020_df$o9b
o24 <- casen2020_df$o24

procesada_o9a <- procesar_glosa(o9a)
procesada_o9b <- procesar_glosa(o9b)
procesada_o24 <- procesar_glosa(o24)

######### Opcional: ver resultado aplicando "procesar_glosa <- function(glosa)" #########

procesada_o9a
procesada_o9b
procesada_o24
