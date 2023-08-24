
##Ejercicio 1

#Construye una función llamada get_cv que calcule el coeficiente de variación 
#de un vector. get_cv debe recibir 2 parámetros: un vector de tamaño n
#un valor boolean que indique si los valores NA deben ser removidos. 
#Por defecto, la función no debe remover dichos valores.
library(dplyr)
get_cv <- function(x,boolean_NA=FALSE){
    n <- length(x)
    coeficiente <- ((sd(x,na.rm = boolean_NA)/mean(x,na.rm = boolean_NA))*100)
    return(coeficiente)
  }

vector <- c(14,10,NA,16,12,NA,10,1,NA)
get_cv(vector, TRUE) 


#Ejercicio 2
#Crea una función llamada build_address que construya una dirección
#en base a algunos inputs, siguiendo un formato establecido.
#build_address recibe 2 parámetros obligatorios y uno opcional.
#Parámetros obligatorios:
#street: string que contiene el nombre de la calle
#number: string que contiene el número de la dirección

build_address <- function(street, number, apartment = NULL) {
  street<-tolower(street)              
  street <-  gsub("avenida |Avenida |AV.|Av. |av |av. |calle |Calle |psj |Psj |Pasaje |pasaje","", street)
  number <-  gsub("[^0-9]","", number) 
  direccion <- paste(street, number, sep = " ")
  if (!is.null(apartment)) {
    apartment <- gsub("[^0-9]", "", apartment) 
    direccion <- paste(direccion, " depto. ", apartment,sep = "")
  }
  return(direccion)
}

street <- "psj Los Alerces"
number <- "número 123"
apartment <- "deptoo 333"
build_address(street, number)
build_address(street, number, apartment)



  
##Utilice la función creada sobre el siguiente dataframe,
#generando una nueva columna llamada dirección.
library(tibble)
df <- tribble(~calle,              ~numero,      ~depto,
              "calle Hemingway",   "num 345",    "depto. 345",
              "av. Albert Camus",  "número 123", "123",
              "Manuel Rojas",      "234",        "departamento 231",
              "Nicanor Parra",     "678",        NULL
) 
df

#Se añade la columna dirección 
df_direccion <- df %>% 
  rowwise() %>%
  mutate(direccion = build_address(calle,numero,depto)) %>% 
  ungroup()

