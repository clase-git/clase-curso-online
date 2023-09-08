library(haven)
library(dplyer)
library(car)
library(cardata)
library(purrr)

# Ejercicio 1 

get_cv <-function(vector,na.rm=FALSE){
  
sd(vector, na.rm=na.rm)/mean(vector,na.rm=na.rm)} #creo funcion vector para calcular cv

###pruebas

p1 <-c(1:15)
get_cv(p1)
#[1] 0.559017

p2 <-c(2,4,6,8,10,70,100)
get_cv(p2)
#[1] 1.385761

p3 <-c(2,4,6,8,10,70,100,NA)
get_cv(p3)
#NA

p3 <-c(2,4,6,8,10,70,100,NA)
get_cv(p3,TRUE)
#1.385761



# Ejercicio 2

library(tidyverse)
library(stringr)
library(dplyr)
library(tibble)
library(stringr)


# Se construye función


build_address <- function(calle, numero, depto = NULL){
  
  
####CALLE
  
calle <- tolower(calle) # a minuscula
calle <- str_remove_all(calle, "(calle) |(street)|(avenida)|(av.)|(pasaje)|(pje)")# limpiar textos
calle <- str_remove_all(calle, "\\.")

####NÚMERO
  
numero <- str_to_lower(numero)
numero <- str_remove_all(numero, ' *') # limpiar textos
numero <- str_extract_all(numero, '\\d+')[[1]]

#### DEPARTAMENTO
  

if(is.null(depto[[1]])){      # manejo de depto para casos NULL y NA
    depto <- ""
    
    direccion <-paste0(calle," ",numero,sep="")
  } 
  
return(direccion)
}


   
#Ejercicio 3
  
  # se copia y prueba df

df <- tibble::tribble(~calle,              ~numero,      ~depto,
                        "calle Hemingway",   "num 345",    "depto. 345",
                        "av. Albert Camus",  "número 123", "123",
                        "Manuel Rojas",      "234",        "departamento 231",
                        "Nicanor Parra",     "678",        NULL)
  df
  

build_address(df$calle[1],df$numero[1])

#[1] "hemingway 345"









