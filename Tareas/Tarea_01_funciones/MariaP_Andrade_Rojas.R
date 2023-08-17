
### EJERCICIO 1 ###

# Parámetros: 
#          x <- vector("numeric", n)
#          Cálculo coef. variación = desviación estándar: sd(x)/ media: mean(x, na.rm)
#          Condición: na.rm = FALSE # NA no deben ser removidos
#
# La función por lo tanto sería:

get_cv <- function(x) {
    sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE)
   }

get_cv()

y <- c(3.5, NA, 7)

get_cv(y)


### EJERCICIO 2 ###


library (stringr)

rm(list = ls())

street <- "calle los alerces"
number <- "numero 123"
apartment <- 34


build_address <- function(street, number, apartment) 
  
{
  st_clean <- gsub("calle|avenida|av.|pasaje|pje.|villa|poblacion|población", "", street) %>% str_trim(side = "left")
  num_clean <- gsub("numero|avenida|av.|av|número|num|n", "", number) %>% str_trim(side = "left")
  apt_clean <- gsub("[a-zA-Z]", "", gsub("\\.", "", apartment)) %>% str_trim(side = "left")
}
{
  st_clean <- gsub("calle|avenida|av.|pasaje|pje.|villa|poblacion|población", "", street) %>% str_trim(side = "left")
  num_clean <- gsub("numero|avenida|av.|av|número|num|n", "", number) %>% str_trim(side = "left")
}

  if (apartment == "")
  {address <- paste(st_clean, num_clean)} else
    
  {
  apt_clean <- gsub("[a-zA-Z]", "", gsub("\\.", "", apartment)) %>% str_trim(side = "left")
  apt_clean2 <- gsub("[0-9]*", ",depto.", apt_clean, ignore.case = TRUE) 
  apt_clean3 <- paste(apt_clean2, apt_clean)
  address <- paste(st_clean, num_clean, apt_clean3)
  paste(address)
  }
 

## Ejercicio 3: Utilice la función creada sobre el siguiente dataframe, generando una nueva columna llamada dirección.

library(tidyverse)

df <- tribble(~calle, ~numero, ~depto,
              "calle Hemingway", "num 345", "depto. 345",
              "av. Albert Camus", "número 123", "123",
              "Manuel Rojas", "234", "departamento 231",
              "Nicanor Parra", "678", NULL)

## Probando con variantes street1, number 1, apartment1 función build_address

street1 <- df$calle
number1 <- df$numero
apartment1 <- df$depto
apartment1 <- rbind(apartment1)
apartment2 <- unlist(apartment1)
print(apartment2)

xx <- 1

build_address <- function(street1, number1, apartment2) 
  
{
  st_clean <- gsub("calle|avenida|av.|pasaje|pje.|villa|poblacion|población", "", street1) %>% str_trim(side = "left")
  num_clean <- gsub("numero|avenida|av.|av|número|num|n", "", number1) %>% str_trim(side = "left")
  apt_clean <- gsub("[a-zA-Z]", "", gsub("\\.", "", apartment2)) %>% str_trim(side = "left")
}
{
  st_clean <- gsub("calle|avenida|av.|pasaje|pje.|villa|poblacion|población", "", street1) %>% str_trim(side = "left")
  num_clean <- gsub("numero|avenida|av.|av|número|num|n", "", number1) %>% str_trim(side = "left")
}

##Si build_address contiene variable con argumento NULL

apartment_l <- is.logical(apartment2) && length(apartment2) == 1 && !is.na(apartment2) && apartment2 

if (isTRUE(apartment_l))
  
{address2 <- paste(st_clean, num_clean)} else
  
{
  apt_clean <- gsub("[a-zA-Z]", "", gsub("\\.", "", apartment2)) %>% str_trim(side = "left")
  apt_clean2 <- gsub("[0-9]*", ",depto.", apt_clean, ignore.case = TRUE) 
  apt_clean3 <- paste(apt_clean2, apt_clean)
  address2 <- paste(st_clean, num_clean, apt_clean3)
  paste(address2)
  dirección <- cbind(paste(address2))
  df <- cbind (df, dirección)
  print (df)
}

