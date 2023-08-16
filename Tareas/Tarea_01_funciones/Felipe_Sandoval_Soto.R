### Tarea 1 Felipe Sandoval

library(dplyr)
library(tibble)
library(stringr)

get_cv <- function(x,y){
  if(anyNA(x)==T){
  if(y==T){
    x<-na.omit(x)
  }else if(y==F){
    stop("No se puede calcular el coeficiente de variación 
         si un valor es NA, debe removerlos o usar TRUE.")
  }else{
    stop("Debe usar un valor booleano como segundo parámetro.")
  }
  if(length(x)<=1){
    stop("No se puede calcular el coeficiente de variación
         de un solo valor.")
  }}
  cv <- (sd(x)/mean(x))
  return(cv)
}

Calle <- function(x) {
  partes <- strsplit(x, " ")
  if (partes[[1]][1]=="avenida"|partes[[1]][1]=="av."|partes[[1]][1]=="calle"
      |partes[[1]][1]=="Calle"|partes[[1]][1]=="Avenida"|partes[[1]][1]=="pasaje"
      |partes[[1]][1]=="psje") {
   C <- paste(partes[[1]][-1], collapse = " ") 
  }else{
   C <- paste(partes[[1]], collapse = " ")
   }
  return(C)
}

Numero <- function(x) {
  partes <- strsplit(x, " ")
  if (partes[[1]][1]=="numero"|partes[[1]][1]=="número"|partes[[1]][1]=="num"
      |partes[[1]][1]=="num."|partes[[1]][1]=="n"|partes[[1]][1]=="n.") {
    N <- paste(partes[[1]][-1], collapse = " ") 
  }else{
    N <- paste(partes[[1]], collapse = " ")
  }
  return(N)
}

Depto <- function(x) {
  partes <- strsplit(x, " ")
  if (partes[[1]][1]=="departamento"|partes[[1]][1]=="depto"|partes[[1]][1]=="depto."
      |partes[[1]][1]=="dpto."|partes[[1]][1]=="dpto"|partes[[1]][1]=="d"|partes[[1]][1]=="d.") {
    D <- paste("depto.",partes[[1]][-1], collapse = " ") 
  }else{
    D <- paste("depto.",partes[[1]], collapse = " ")
  }
  return(D)
}

build_address<-function(x,y,z=NULL){
  C<-Calle(x)
  N<-Numero(y)
  if (is.null(z)==T) {
    direccion<-paste(C,N)
  } else{
  D<-Depto(z)
  direccion<-paste(C,paste0(N,","),D)
  }
  return(direccion)
}

df <- tribble(~calle,              ~numero,      ~depto,
              "calle Hemingway",   "num 345",    "depto. 345",
              "av. Albert Camus",  "número 123", "123",
              "Manuel Rojas",      "234",        "departamento 231",
              "Nicanor Parra",     "678",        NULL
) 

df<- df %>% 
      rowwise() %>%
      mutate(direccion = build_address(calle,numero,depto)) %>% 
      ungroup()

