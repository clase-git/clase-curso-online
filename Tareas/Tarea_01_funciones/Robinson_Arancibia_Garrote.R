library(dplyr)
library(tibble)

#EJERCICIO N°1

# Construye una función llamada get_cv que calcule el coeficiente de variación 
# de un vector. get_cv debe recibir 2 parámetros:
# un vector de tamaño n
# un valor boolean que indique si los valores NA deben ser removidos. 
# por defecto, la función no debe remover dichos valores.
# En la construcción de la función no está permitido utilizar 
# la función cv, pero sí mean y sd

get_cv <- function(vector, eliminar_NA = F) {
  if (eliminar_NA) {
    vector <- vector[!is.na(vector)]
  }
  
media <- mean(vector)
desviacion_estandar <- sd(vector)
  
  if (media == 0) {
    return(NA)
  }
  
coeficiente_variacion <- (desviacion_estandar / media) * 100
  return(coeficiente_variacion)
}


# Ejemplos 
Ejemplo_1 <- c(1:50,10)
Ejemplo_2 <- c(1:30,10,NA)

cv_1 <- get_cv(Ejemplo_1)
cv_2 <- get_cv(Ejemplo_2, eliminar_NA = TRUE)


#EJERCICIO N°2 

# Crea una función llamada build_address que construya una dirección en base
# a algunos inputs, siguiendo un formato establecido. build_address 
# recibe 2 parámetros obligatorios y uno opcional.
# Parámetros obligatorios:
# street: string que contiene el nombre de la calle
# number: string que contiene el número de la dirección
# El parámetro opcional es apartment y contiene el número del departamento.
# A continuación se muestra un ejemplo del resultado esperado


#street <- "calle Los Alerces"
# number <- "número 123"
# build_address(street, number)
## [1] "los alerces 123"



build_address <- function(street, number, apartment = NULL) {
  
street <- gsub("^(calle|avenida|av\\.|pasaje) ", "", tolower(street))
  
number <- gsub("^(num(ero)?|n(úmero)?) ", "", tolower(number))
  
# Construir la dirección
  
  if (!is.null(apartment)) {
    
    direccion <- paste(street, number, apartment, sep = ", ")
    
  } else {
    
    direccion <- paste(street, number, sep = ", ")
    
  }
  
  
return(direccion)
  
}


# Ejemplos

street <- "calle Los Alerces"
number <- "número 123"
ejemplo1 <- build_address(street, number)
print(ejemplo1)  

street <- "avenida 21 de Mayo"
number <- "num 1442"
apartment <- "2"


ejemplo2 <- build_address(street, number, apartment)
print(ejemplo2)  
street <- "pasaje Ananías Bruna"
number <- "n 45"

ejemplo3 <- build_address(street, number)
print(ejemplo3)  



#EJERCICIO 3 

# Utilice la función creada sobre el siguiente dataframe, 
# generando una nueva columna llamada dirección.

# Creación del df
df <- tribble(
  ~calle,              ~numero,      ~depto,
  "calle Hemingway",   "num 345",    "depto. 345",
  "av. Albert Camus",  "número 123", "123",
  "Manuel Rojas",      "234",        "departamento 231",
  "Nicanor Parra",     "678",        NULL
)


df <- df %>%
  mutate(direccion = build_address(calle, numero, depto))

df$direccion <- mapply(build_address, df$calle, df$numero, df$depto)


#Mostrar el DataFrame resultante
print(df)
