#### TAREA 3, Denis Lopez Calzadilla ######

# Ejercicio 1

library(feather)
library(tidyverse)

casen<- read_feather("C:/Users/Victor Segura/Desktop/Tarea 3/casen_2020_edit.feather")

names(casen)

#### Ocupación

DF_ocu <- casen %>%
  select(matches("^o((\\d+)|ficio|cup)|^activ|^rama"))

names(DF_ocu)


#### Vivienda

#Selecionar el Modulo de vivienda

DF_vivienda <- casen %>%
  select(matches("^v\\d+"))

names(DF_vivienda)




variables_ocupacion <- casen %>% 
  select(matches("ocup.*"))

variables_vivienda <- casen %>% 
  select(matches("viv.*"))


# Seleccionar variables del módulo de Ocupación
casen_ocupacion <- casen %>%
  select(matches("^o\\d+|^of|^rama"))

# Seleccionar variables del módulo de Vivienda
casen_vivienda <- casen %>%
  select(matches("^v\\d+"))

# Mostrar las primeras filas de las tablas resultantes
head(casen_ocupacion)
head(casen_vivienda)


# Ejercicio 2

Glosa_limpia <- function(chr_var) {
  chr_var <- as.character(chr_var)
  chr_var <- tolower(chr_var) #minusculas
  chr_var <- str_replace_all(chr_var, "[[:punct:]]", "")#remueve signos de puntuacion y caracteres especiales
  chr_var <- str_replace_all(chr_var, "[0-9]", "")#remueve números
  chr_var <- str_replace_all(chr_var, "\\s+", " ") # # Extraer espacios adicionales entre palabras
}


casen$Nva_ocup<- Glosa_limpia(casen$o9a)
#table(casen$Nva_ocup)
casen$Tareas_ocup<- Glosa_limpia(casen$o9b)
#table(casen$Tareas_ocup)
casen$Ram_ocu<- Glosa_limpia(casen$o24)
#table(casen$Ram_ocu)
