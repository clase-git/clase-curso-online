rm(list = ls()) #Limpia variables

###Ejercicio 1###

#install.packages("feather")

library(feather)
library(dplyr)
library(stringr)

Casen2020 <- read_feather("C:/Users/Capineda/Desktop/Curso R intermedio/casen_2020_edit.feather")

data_o <- Casen2020 %>%
  select_if(str_detect(colnames(.), "^o")) 

data_v <- Casen2020 %>%
  select_if(str_detect(colnames(.), "^v")) 

print("Tabla para nombres que comienzan con 'o':")
print(data_o)

print("Tabla para nombres que comienzan con 'v':")
print(data_v)

###Ejercicio 2###
#install.packages("tm")
library(tm)

data_02 <- data_o%>% 
  select (o9a, o9b, o24)

procesada <- function(texto) {
  texto <- tolower(texto) #
  texto <- str_replace_all(texto, "[[:punct:]]", "") 
  texto <- str_replace_all(texto, "[[:digit:]]", "")
  stopwords <- stopwords("es")
  texto <- removeWords(texto, stopwords)
  texto <- str_replace_all(texto, "\\s+", " ")
  texto <- trimws(texto)
  return(texto)
}

data_02 %>% mutate(procesada1=procesada(o9a),procesada2=procesada(o9b),procesada3=procesada(o24) )

