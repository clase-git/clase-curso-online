#Tarea de la clase del 23-08-2023

library(feather)
library(tidyverse)
library(readr)
library(dplyr)
library(stringr)


#EJERCICIO 1

casen <- read_feather("casen_2020_edit.feather")

#Módulo ocupación

columnas_seleccionadas_o <- colnames(casen)[str_detect( colnames(casen),"^(o+(\\d{1}|\\d{2})|oficio|rama)")]
Modulo_ocupacion <- casen[, columnas_seleccionadas_o]
view(Modulo_ocupacion)

#Módulo vivienda

columnas_seleccionadas_v <- colnames(casen)[str_detect( colnames(casen),"^v+(\\d{1}|\\d{2})")]
Modulo_vivienda <- casen[, columnas_seleccionadas_v]
view(Modulo_vivienda)

#EJERCICIO 2

procesamiento_glosas <- function(data,var){
  data %>% 
    mutate(
      variable = {{ var }},
      variable = tolower( variable ),
      variable = str_replace_all( variable,pattern = "[[:punct:]]",replacement = ""),
      variable = str_replace_all( variable,pattern = "\\d+",replacement = ""),
      variable = str_replace_all( variable,pattern = "\\s+",replacement = " ")
    ) 
}

caso1 <- procesamiento_glosas(casen,o9a)
caso2 <- procesamiento_glosas(casen,o9b)
caso3 <- procesamiento_glosas(casen,o24)




