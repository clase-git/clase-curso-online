library(dplyr)
#TAREA
#Ejercicio 1
get_cv<-function(vector, remover_na=FALSE) {
  
  if(remover_na){
    vector<-vector[!is.na (vector)]
  }
  if(length(vector)<=1){
    stop("El vector debe tener al menos dos valores para calcular el coeficiente de variación")
  }
  
  cv<-sd(vector)/mean(vector)
}

mi_vector<-c(10,15,8,NA,12,9)
CV<-get_cv(mi_vector,remover_na=TRUE)
print(CV)

#Ejercicio 2
build_address <- function(calle, numero, departamento = NULL) {
  
  texto <- calle
  palabra1 <- "Calle"
  palabra2 <- "calle"
  palabra3 <- "Avenida"
  palabra4 <- "avenida"
  palabra5 <- "Av."
  palabra6 <- "av."
  palabra7 <- "Pasaje"
  palabra8 <- "pasaje"
  
  calle <-  gsub(paste(palabra1, "|", palabra2, "|", palabra3, "|", palabra4,"|", palabra5, "|", palabra6, "|", palabra7, "|", palabra8, sep = ""), "", texto)
  
  
  texto1 <- numero
  p1 <- "numero"
  p2 <- "número"
  p3 <- "num"
  p4 <- "n"
  p5 <- "Av."
 
  numero <-  gsub(paste(p1, "|", p2, "|", p3, "|", p4,"|", p5, sep = ""), "", texto1)
  
  texto2 <- departamento
  p11 <- "departamento"
  p12 <- "depto"
  p13 <- "depto."
  p14 <- "NULL"
  p15 <- "dpto"
  
  departamento <-  gsub(paste(p11, "|", p12, "|", p13, "|", p14,"|", p15, sep = ""), "", texto2)
  
  if (is.null(departamento)) {
    direccion <- data.frame(paste(calle, numero))
  } else {
    direccion <- data.frame(paste(calle, numero, "dpto.", departamento))
  }
  
  return(direccion)
}

# Ejemplos de uso:
direccion_1 <- build_address("Calle Los Alerces", "número 123")
direccion_2 <- build_address("Avenida Los Alerces", 456, departamento = NULL)

print(direccion_1)
print(direccion_2)


df <- tribble(~calle,              ~numero,      ~depto,
              "calle Hemingway",   "num 345",    "depto. 345",
              "av. Albert Camus",  "número 123", "123",
              "Manuel Rojas",      "234",        "departamento 231",
              "Nicanor Parra",     "678",        NULL
) 
df
#Ejercicio 3
df1<-build_address(df$calle,df$numero,df$depto)
