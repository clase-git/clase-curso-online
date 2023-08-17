install.packages("qdapRegex")

library(dplyr)
library(qdapRegex) # para utilizar "rm_between_multiple", no logré encontrar otra función que haga lo mismo de una manera mas fácil

#==========  Ejercicio 1  ============#

get_cv <- function(x, na.rm = FALSE) {          # declara función con dos parámetros, x:vector na.rm: boolean para remover NA
  desviacion_s <- sd(x, na.rm)                  # se utiliza formula de desviación estándar con opción de remover NA
  media <- mean(x, na.rm = na.rm)               # se calcula promedio con opción de remover NA
  cvar <- (desviacion_s/abs(media))             # calcula por formula el coeficiente de variación
  return(cvar)                                  # devuelve resultado
}

# Ejemplo:
datos <- c(3450,3550, 3650, 3480, 3355, 3310, 3490, 3730, 3540, 3925, 3520, 3480 )
get_cv(datos)



#==========  Ejercicio 2  ============#

build_address <- function(street, number, apartment = NULL) {                   # declara función con 3 parámetros -calle, numero y departamento (opcional)
  
  banned_str_left <- c("av.", "avenida", "calle", "pasaje")                     # se crea vector con las palabras que se deben quitar de street (lado izquierdo) (se pueden agregar mas)
  banned_str_right <- c(" ", " ", " ", " ")                                     # se crea vector auxiliar son espacios para cerrar la búsqueda desde lado derecho
  street <- rm_between_multiple(street, banned_str_left, banned_str_right)      # se remueve los caracteres que se encuentran en street, desde la cadena de izquierda hasta la cadena derecha 
  
  banned_num_left <- c("numero", "num", "número", "n")                          # se crea vector con las palabras que se deben quitar de number (lado izquierdo) (se pueden agregar mas)
  banned_num_right <- c(" ", " ", " ", " ")                                     # se crea vector auxiliar son espacios para cerrar la busqueda desde lado derecho
  number <- rm_between_multiple(number, banned_num_left, banned_num_right)      # se remueve los caracteres que se encuentran en number, desde la cadena de izquierda hasta la cadena derecha 
  
  banned_apart_left <- c("depto.", "departamento")                              # se crea vector con las palabras que se deben quitar de apartment (lado izquierdo) (se pueden agregar mas)
  banned_apart_right <- c(" ", " ")                                             # se crea vector auxiliar son espacios para cerrar la búsqueda desde lado derecho
  apartment <- ifelse(rm_between_multiple(apartment, banned_apart_left, banned_apart_right)=="character(0)",
                      "",paste(", depto.",rm_between_multiple(apartment, banned_apart_left, banned_apart_right)))   # se remueve los caracteres que se encuentran en apartment, desde la cadena de izquierda hasta la cadena derecha
                                                                                                                    # si rm_between_multiple devuelve "character(0)" (por un NULL), se reemplaza por "" 
  
  return(paste (street," ",number, apartment, sep = ""))                        # devuelve cadena que une street, number y apartment
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
  mutate ( direccion = build_address(df$calle, df$numero, df$depto))            # crea dataframe en base a df y se agrega una columna con la función build_address llamada direccion


