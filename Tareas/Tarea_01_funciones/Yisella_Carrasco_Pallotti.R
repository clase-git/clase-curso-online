##############################################################################################################
#Ejercicio 1

#Construye una función llamada get_cv que calcule el coeficiente de variación de un vector.
#get_cv debe recibir 2 parámetros:
  
#un vector de tamaño n
#un valor boolean que indique si los valores NA deben ser removidos. 
#Por defecto, la función no debe remover dichos valores.

#En la construcción de la función no está permitido utilizar la función cv, pero sí mean y sd

#La fórmula para calcular el coeficiente de variación es cv = sd/X

##############################################################################################################
library(dplyr)


get_cv <- function(x,bool=FALSE) { #bool se debe cambiar de forma manual
  sd <- sd(x,na.rm=bool) # distancias respecto a la media
  media <- mean(x,na.rm=bool) # suma de cuadrados
  cv=(sd/media)*100
  cv
}

set.seed(123)
vector <- rnorm(n = 10000)
get_cv(vector)
##############################################################################################################

#Ejercicio 2

#Crea una función llamada build_address que construya una dirección en base a algunos inputs, siguiendo un formato establecido. 
#build_address recibe 2 parámetros obligatorios y uno opcional.

#Parámetros obligatorios:

#street: string que contiene el nombre de la calle
#number: string que contiene el número de la dirección

#El parámetro opcional es apartment y contiene el número del departamento.

##############################################################################################################
library(stringr)


build_address <- function(street,number,apartment){
  if (!is.null(apartment)){
    calle <- gsub("\\b[a-z]+\\b", "",street)
    calle <- gsub("\\.", "",calle)
    calle <- str_trim(calle, "left")
    numero <- str_extract(number, "\\d+")
    depto <- str_extract(apartment, "\\d+")
    address <- paste(tolower(calle),numero,",","depto.",depto)
  }else{
    calle <- gsub("\\b[a-z]+\\b", "",street)
    calle <- gsub("\\.", "",calle)
    calle <- str_trim(calle, "left")
    numero <- str_extract(number, "\\d+")
    depto <- str_extract(apartment, "\\d+")
    address <- paste(tolower(calle),numero)
  }
  address
  
}

street <- "calle Los Alerces"
number <- "número 123"
apartment <- NULL
build_address(street,number,apartment)
##############################################################################################################

#Ejercicio 3
#Utilice la función creada sobre el siguiente dataframe,
#generando una nueva columna llamada dirección.

##############################################################################################################
df <- dplyr::tribble(~calle,              ~numero,      ~depto,
              "calle Hemingway",   "num 345",    "depto. 345",
              "av. Albert Camus",  "número 123", "123",
              "Manuel Rojas",      "234",        "departamento 231",
              "Nicanor Parra",     "678",        NULL
) 

df <- df %>% rename (street = calle, number = numero, apartment = depto)

df

#hasta ahora este es el mejor

df<- df %>% 
  rowwise() %>%
  mutate(dirección = build_address(street,number,apartment))%>% 
  ungroup()

df <- df %>% rename (calle=street, numero=number, depto=apartment)

df$dirección
###################################### COMENTARIO FINAL ###################################################

# el fallo del código es cuando se ingresa una dirección con minúscula
