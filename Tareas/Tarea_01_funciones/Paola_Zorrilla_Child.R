
library(dplyr)
library(tibble)

#Ejercicio 1----

get_cv <- function(vector, elimiNA = FALSE) {
  if (elimiNA) {
    vector <- vector[!is.na(vector)]
  }
  
  if (any(is.na(vector))) {
    stop ("Existe un valor NA dentro del vector, modifique los parametros de la función a get_cv(vector, TRUE)")
  }
  
  if (length(vector) <= 1) {
    stop("El vector debe contar minimo con dos valores para poder calcular el coeficiente de variación.")
  }
  if (mean(vector) == 0) {
    stop("El promedio de los datos del vector es cero, por lo que no se puede calcular el coeficiente de variación.")
  }
  
  valor_sd <- sd(vector)
  valor_media <- mean(vector)
  cv <- valor_sd / valor_media
  
  return(cv)
}

#Ejemplos

a<-c(2,3,4,5,6,7,NA,8,9,10,11,12)
b<-c(-3,-2,-1,1,2,3)
c<-c(1)

get_cv(a) # Error in get_cv(a) : Existe un valor NA dentro del vector, modifique los parametros de la función a get_cv(vector, TRUE)
get_cv(a, TRUE) #[1] 0.4738035
get_cv(b) # Error in get_cv(b) : El promedio de los datos del vector es cero, por lo que no se puede calcular el coeficiente de variación.
get_cv(c) # Error in get_cv(c) : El vector debe contar con al menos dos valores para poder calcular el coeficiente de variación.


#Ejercicio 2 ----

build_address <- function(street, number, apartment = NULL) {
  street <- tolower(street)
  street <- gsub("avenida |av. |av |calle |pasaje |pje", "", street)
  number <- gsub("[^0-9]", "", number)  
  address <- paste(street, number, sep = " ")
  
  # Agregar departamento si está presente
  if (!is.null(apartment)) {
    apartment <- gsub("[^0-9]", "", apartment) 
    address <- paste(address, ", depto. ", apartment,sep = "")
  }
  return(address)
}

# Ejemplos
street <- "calle Los Alerces"
number <- "número 123"
apartment <- "departamento 105"

build_address(street, number)
build_address(street, number, apartment)


#Ejercicio 3 ----

df <- tribble(~calle,              ~numero,      ~depto,
              "calle Hemingway",   "num 345",    "depto. 345",
              "av. Albert Camus",  "número 123", "123",
              "Manuel Rojas",      "234",        "departamento 231",
              "Nicanor Parra",     "678",        NULL
) 
df

df<- df %>%
  dplyr::rowwise() %>%
  dplyr::mutate(direccion=build_address(calle, numero, depto)) %>%
ungroup()

df
