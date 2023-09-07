
library(tm)
library(quanteda)

#Crear función que reciba argumento variable character y la procese según condiciones dadas:

procesar_glosa <- function(glosa) {
  
  gl_proc <- glosa %>% 
    
    tolower() %>% #Convertir todo a minúscula
  
    gsub("[[:punct:]]", "", .) %>% #Remover puntuación y caracteres especiales
  
   gsub("[[:digit:]]", "", .)  %>% #Remover números
  
  gsub("\\s+", " ", .)  %>% #Extraer espacios adicionales
  
  tokens() %>% #Partición del texto para facilitar proceso
  
  stopwords("es")  %>% #Se escoge español para no obtener errores, no pude encontrar alternativa
  
  tokens_remove(stopwords("es"))  %>% #Remover stopwords (palabras sin significado)
  
  as.character()
  
  return(gl_proc)
}

#Variables solicitadas a procesar

o9a <- casen2020_df$o9a
o9b <- casen2020_df$o9b
o24 <- casen2020_df$o24

#Opcional: resultado aplicando "procesar_glosa <- function(glosa)":

procesada_o9a <- procesar_glosa(o9a)
procesada_o9b <- procesar_glosa(o9b)
procesada_o24 <- procesar_glosa(o24)
procesada_o9a
procesada_o9b
procesada_o24
