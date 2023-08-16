install.packages("qdapRegex")

library(dplyr)
library(qdapRegex)

#==========  Ejercicio 1  ============#

get_cv <- function(x, na.rm = FALSE) {
  desviacion_s <- sd(x, na.rm)
  media <- mean(x, na.rm = na.rm)
  cvar <- (desviacion_s/abs(media))
  return(cvar)
}

# Ejemplo:
datos <- c(3450,3550, 3650, 3480, 3355, 3310, 3490, 3730, 3540, 3925, 3520, 3480 )
get_cv(datos)



#==========  Ejercicio 2  ============#

build_address <- function(street, number, apartment = NULL) {
  
  banned_str_left <- c("av.", "avenida", "calle", "pasaje")
  banned_str_right <- c(" ", " ", " ", " ")
  street <- rm_between_multiple(street, banned_str_left, banned_str_right)
  
  banned_num_left <- c("numero", "num", "número", "n")
  banned_num_right <- c(" ", " ", " ", " ")
  number <- rm_between_multiple(number, banned_num_left, banned_num_right)  
  
  banned_apart_left <- c("depto.", "departamento")
  banned_apart_right <- c(" ", " ")
  apartment <- ifelse(rm_between_multiple(apartment, banned_apart_left, banned_apart_right)=="character(0)",
                      "",paste(", depto.",rm_between_multiple(apartment, banned_apart_left, banned_apart_right)))
  
  return(paste (street," ",number, apartment, sep = ""))
}

# Ejemplo 1:
street <- "calle Los Alerces"
number <- "número 123"
build_address(street, number)

# Ejemplo 2:
street <- "calle Los Alerces"
number <- "número 123"
apartment <- "departamento 34"
build_address(street, number, apartment)


#==========  Ejercicio 3  ============#

df <- tribble(~calle,              ~numero,      ~depto,
              "calle Hemingway",   "num 345",    "depto. 345",
              "av. Albert Camus",  "número 123", "123",
              "Manuel Rojas",      "234",        "departamento 231",
              "Nicanor Parra",     "678",        NULL
) 

direccion <- df %>%
  mutate ( direccion = build_address(df$calle, df$numero, df$depto))


