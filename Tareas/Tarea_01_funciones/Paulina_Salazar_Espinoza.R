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
