# Resolución de ejercicios

# Ejercicio 1
# Construye una función llamada get_cv que calcule el coeficiente de variación de un vector. get_cv debe recibir 2 parámetros:
# un vector de tamaño n
# un valor boolean que indique si los valores NA deben ser removidos. Por defecto, la función no debe remover dichos valores.
# En la construcción de la función no está permitido utilizar la función cv, pero sí mean y sd


# función calcula coeficiente de variación
get_cv <- function(vector, valor=FALSE) {
  if (valor == TRUE) {valor <-  TRUE} 
  cv <- sd(vector,na.rm = valor) / mean(vector,na.rm = valor) * 100  
  return(cv)
}

# ejemplos de prueba
n <- c (67, 68, 68, 74, 74, 76, 76, 77, 78, NA )
get_cv(n,TRUE)

n <- c (88, 85, 82, 97, 67, 77, 74, 86, 81, 95)
get_cv(n)


# Ejercicio 2
# Crea una función llamada build_address que construya una dirección en base a algunos inputs, siguiendo un formato establecido. build_address recibe 2 parámetros obligatorios y uno opcional.
# Parámetros obligatorios:
# street: string que contiene el nombre de la calle
# number: string que contiene el número de la dirección
# El parámetro opcional es apartment y contiene el número del departamento.
#

library(tidyverse)
library(stringr)

# función construye dirección
build_address <- function(street, number, apartment=NULL){
  # para extraer nombre de la calle
  patron_calle <- '(calle|Calle|CALLE|Avenida|AVENIDA|Avda.|av.|av|Av.|Av|AV|avenida|AVDA.|AVDA|pasaje|Pasaje|psje|psje.|PSJE.|PASAJE)'
  nombre_calle <- unlist(strsplit(street, patron_calle))  # separo palabras y dejo fuera las palabras definidas en patrón
  nombre_calle <- nombre_calle[grep("[[:alpha:]]", nombre_calle)]  # Filto solo las palabras con letras
  # para extraer número de calle
  numero_calle <- as.numeric(gsub("\\D", "", number))   # elimino caracteres que no son dígitos, dejo solo números
  if (is.null(apartment)) {
    direccion <- paste(nombre_calle, numero_calle, sep = " ")
    print(direccion)
  } else {
    numero_depto <- as.numeric(gsub("\\D", "", apartment))
    direccion <- paste(nombre_calle, numero_calle,", depto.", numero_depto, sep = " ")
    print(direccion)
  }
}

# ejemplos de prueba
street <- "AVDA. los loros"
number <- "número 345"
build_address(street, number)

street <- "avenida los loros"
number <- "número 345"
depto <- "depto. 45"
build_address(street, number, depto)

street <- "avenida los loros"
number <- "número 345"
depto <- NULL
build_address(street, number, depto)


# Ejercicio 3
# Utilice la función creada sobre el siguiente dataframe, generando una nueva columna llamada dirección.

df <- tribble(~calle,              ~numero,      ~depto,
              "calle Hemingway",   "num 345",    "depto. 345",
              "av. Albert Camus",  "número 123", "123",
              "Manuel Rojas",      "234",        "departamento 231",
              "Nicanor Parra",     "678",        NULL
) 

# se genera en df nueva columna con dirección
df <- df %>% 
  rowwise() %>%
  mutate(dirección = build_address(calle, numero, depto)) %>% 
  ungroup()



