install.packages("stopwords")
install.packages("qdap")
install.packages("tm")

library(feather)
library(tidyverse)


casen = read_feather("casen_2020_edit.feather")
casen <- head(casen,100) # para un procesamiento liviano 

################## Ejercicio 1 ##################

# opción 1
mod_ocup_vivd_1 <- casen %>%
                select(
                        grep("^o\\d+", colnames(casen)), # selecciona todas que comienzan con 'o' y le sigue uno o más números
                        grep("^v\\d+", colnames(casen)) # selecciona todas que comienzan con 'v' y le sigue uno o más números
                        )

# opción 2
mod_ocup_vivd_2 <- casen %>%
                select(
                        which(str_detect(colnames(casen), "^o\\d+")),  # selecciona todas que comienzan con 'o' y le sigue uno o más números
                        which(str_detect(colnames(casen), "^v\\d+"))  # selecciona todas que comienzan con 'v' y le sigue uno o más números
                        )
  
  
################## Ejercicio 2 ##################

library(stopwords)
library(qdap)
library(tm)

procesar <- function(var){
  var <- var %>%
        tolower() %>%                         # convierte a minúsculas
        str_replace_all("[:punct:]","") %>%   # quita caracteres de puntuación y especiales:!»#$%&‘()*+,–./:;<=>?@[\]^_`{|}~
        str_remove_all("[0-9]") %>%           # remueve números
        str_replace_all("\\s{2,}"," ") %>%    # reemplaza los espacios dobles o más por solo uno
        str_remove_all("^\\s|\\s$")      # remueve los espacios de inicio y final
  
  var <- rm_stopwords(var, stopwords("es"))[] # remueve los stopwords en español
  var <- map(var,~paste(.x, collapse = " ")) # reúne cada palabra en una cadena de cada elemento de la lista
  
  return(var)
          }


test <- tibble(                          # probando función
                procesar(casen$o9a),
                procesar(casen$o9b),
                procesar(casen$o24)
                )

