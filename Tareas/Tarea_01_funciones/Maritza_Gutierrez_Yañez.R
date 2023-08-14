#Tarea clase 08-08-2023 

library(tidyverse)
library(stringr)

#Ejercicio 1


get_cv <- function(x){
  n <- length(x)
  cv <- ((sd(x,na.rm = TRUE)/mean(x,na.rm = TRUE))*100)
  return(cv)
}



#Ejercicio 2

build_address <- function(street,number,apartment=NULL){
  numero <- ifelse(grepl("[0-9]+", number), str_extract(number, "[0-9]+"), "")
  patron_calle <- '(calle|Calle|CALLE|Av.|AV.|AV|av|av.|Avenida|avenida|AVENIDA|pasaje|PASAJE|Pasaje|psj.)'
  calle <-  gsub(patron_calle,c(""), street) # Reemplaza todas las coincidencias
     
  if (is.null(apartment)){
      direccion <- paste(calle, numero, sep = " ")
      return(direccion)
    }
  else{
    departamento <- ifelse(grepl("[0-9]+", apartment), str_extract(apartment, "[0-9]+"), "")
    direccion2 <- paste(calle, numero, ", depto.", departamento, sep = " ")
    return(direccion2)
  }
}



street <- "calle Los Alerces"
number <- "numero 123"

build_address(street,number)



#Ejercicio 3

df <- tribble(~calle,              ~numero,      ~depto,
              "calle Hemingway",   "num 345",    "depto. 345",
              "av. Albert Camus",  "número 123", "123",
              "Manuel Rojas",      "234",        "departamento 231",
              "Nicanor Parra",     "678",        NULL
) 
df


df2 <- df %>%
  rowwise() %>%
  mutate(direccion= build_address(street = calle, number = numero,apartment = depto))

















































