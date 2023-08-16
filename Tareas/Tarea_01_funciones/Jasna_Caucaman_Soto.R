### Tarea Curso R Intermedio
library(dplyr)
library(tibble)
library(stringr)

## Ejercicio 1

get_cv <- function(vectorx, remover_NA=FALSE){
  if(length(vectorx)<1){
    stop("El vector debe contener más de un valor")
  }
  if(remover_NA){
    vectorx<- vectorx[!is.na(vectorx)]
  }
  coef_var <- (sd(vectorx)/mean(vectorx)*100)
  return( coef_var)
}


vector1 <- c(10,12,15,14,25,NA,34,30)
cv<- get_cv(vector1, remover_NA=TRUE)
print(cv)


## Ejercicio 2

# Se crea la funcion
build_address <- function(street, number, apartment=NULL) {
  
  # Función para extraer el nombre de la calle en minuscula
  street_name <- function(street) {
    cleaned_street <- gsub("\\b(av\\.?|Pa\\.?|Ca\\.?|avenida|calle|paseo|pasaje|plaza)\\s+", "", street, ignore.case=TRUE)
    return(tolower(cleaned_street))
  }
  
  # Función para extraer el número de la dirección
  extract_number <- function(number) {
    clean_number <- regexpr("\\b\\d+\\b", number)
    if (clean_number > 0) {
      return(substr(number, clean_number, clean_number + attr(clean_number, "match.length") - 1))
    }
    return(NULL)
  }
  
  
  cleaned_street <- street_name(street)
  clean_number <- extract_number(number)
  #apartment <- str_extract(apartment, "\\d+")
  
  direccion <- c(cleaned_street, clean_number)
  if(!is.null(apartment)){
    apartmentx <- str_extract(apartment, "\\d+")
  }
  
  if (!is.null(apartment)) {
    direccion <- c(direccion, paste(", depto.", apartmentx))
  }
  
  address <- paste(direccion, collapse = " ")
  return(address)
}






#Ejemplo1
street <- "Calle Los Alerces"
number <- "número 123"

direccion1 <- build_address(street, number)
print(direccion1)

#Ejemplo2
street <- "Avenida Mirasol"
number <- "num 570"
apartment <- "departemento 52"


direccion2 <- build_address(street, number, apartment)
print(direccion2)

#Ejemplo3
direccion3 <- build_address("Pasaje LOS AROMOS","número 70","3")
print(direccion3)


## Ejercicio 3

#Se crea el dataframe
df <- tribble(~street, ~number, ~apartment,
              "calle Hemingway", "num 345", "depto. 345",
              "av. Albert Camus", "número 123", "123",
              "Manuel Rojas", "234", "departamento 231",
              "Nicanor Parra", "678", NULL)

#Se añade la columna dirección 
df <- df %>%
  mutate(dirección = mapply(build_address, street, number, apartment))



