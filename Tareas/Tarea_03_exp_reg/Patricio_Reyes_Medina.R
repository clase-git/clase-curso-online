
# library(feather)
# library(tidyverse)
# 
# casen = read_feather("data/casen_2020_edit.feather")

# Ejerciocio 1 

# Variables modulo ocupación
tidyr::tibble(
  variables_mo = stringr::str_subset(names(casen), pattern = "^o\\d"))

# Variables modulo vivienda
tidyr::tibble(
  variables_mv = stringr::str_subset(names(casen), pattern = "^v\\d"))


# Ejercicio 2

fun_remove <- function(variable){
  variable_r <- stringr::str_to_lower(variable)
  variable_r <- stringr::str_replace_all(variable_r, pattern = "[:punct:]", replacement = " ")
  variable_r <- iconv(variable_r, to = "ASCII//TRANSLIT")
  variable_r <- stringr::str_remove_all(variable_r, "[0-9]")
  variable_r <- tm::removeWords(variable_r, tm::stopwords("es"))
  variable_r <- stringr::str_replace_all(variable_r, "\\s+", " ")
  return(variable_r)
}































