# Ejercicio 1
library(dplyr)

get_cv <- function(vector, remover_na = FALSE){
  if (remover_na) {
    vector <- vector[!is.na(vector)]
  }
  
  mean_value <- mean(vector)
  sd_value <- sd(vector)
  cv_value <- (sd_value / mean_value) * 100
  return(cv_value)
}


# Ejercicio 2

build_address <- function(street, number, apartment = NULL) {
  cleaned_street <- gsub("^(av\\.|avenida|calle|pasaje)\\s*", "", tolower(street))
  
  cleaned_number <- gsub("^(numero|num|número|numer)\\s", "", tolower(number))
  
  address <- paste(cleaned_street, cleaned_number, sep = " ")
  
  if (!is.null(apartment)) {
    cleaned_apartment <- gsub("^(departamento|depto.|depto|dpto.|dpto)\\s*", "depto. ", tolower(apartment))
    address <- paste0(address, ", ", cleaned_apartment)
  }
  
  return(address)
}

# ejercicio 3

df <- tribble(~calle, ~numero, ~depto,
              "calle Hemingway", "num 345", "depto. 345",
              "av. Albert Camus", "número 123", "123",
              "Manuel Rojas", "234", "departamento 231",
              "Nicanor Parra", "678", NULL
)

df %>% 
  mutate(direccon = build_address(calle, numero, depto))



