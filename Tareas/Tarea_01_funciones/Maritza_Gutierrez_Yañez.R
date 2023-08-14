#Tarea clase 08-08-2023 

library(tidyverse)
library(stringr)


#Ejercicio 1


#OPCION 1, función que no remueve los valores NA 

get_cv <- function(x,valor_na=FALSE){
  n <- length(x)
  cv <- ((sd(x,na.rm = valor_na)/mean(x,na.rm = valor_na))*100)
  return(cv)
}

prueba <- c(1,5,8,NA,12,14)
get_cv(prueba, TRUE) # hay que incorporar el TRUE para poder calcular el CV


#OPCION 2, función que remueve los valores NA

get_cv2 <- function(x){
  n <- length(x)
  cv <- ((sd(x,na.rm = TRUE)/mean(x,na.rm = TRUE))*100)
  return(cv)
}

get_cv2(prueba)




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
              "av. Albert Camus",  "n?mero 123", "123",
              "Manuel Rojas",      "234",        "departamento 231",
              "Nicanor Parra",     "678",        NULL
) 
df


df2 <- df %>%
  rowwise() %>%
  mutate(direccion= build_address(street = calle, number = numero,apartment = depto))

















































