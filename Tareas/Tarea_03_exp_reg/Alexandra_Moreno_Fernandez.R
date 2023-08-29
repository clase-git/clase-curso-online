library(feather)
library(tidyverse)
library(dplyr)

#Ejercicio1
casen<- read_feather("C:/Users/alexa/Desktop/clase_3/casen_2020_edit.feather")

variables_ocupacion <- casen %>% 
  select(matches("ocup.*"))

variables_vivienda <- casen %>% 
  select(matches("viv.*"))

View(variables_ocupacion)

View(variables_vivienda)

#Ejercicio 2
install.packages("tm")
library(tm)
library(stringr)

procesar_variable <- function(variable) {
  
  variable <- tolower(variable)
  
  variable <- gsub("[[:punct:]]", "", variable)
  
  variable <- gsub("[[:digit:]]", "", variable)
  
  variable <- gsub("\\s+", " ", variable)
  
  stop_words <- stopwords("es")
  variable <- removeWords(variable, stop_words)
  
  return(variable)
}
