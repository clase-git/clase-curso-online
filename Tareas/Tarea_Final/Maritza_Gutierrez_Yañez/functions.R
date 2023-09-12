#FUNCIONES GENERADAS PARA LA TAREA PRINCIPAL

#EJERCICIO 1

#1.1
#Crear una función llamada extract_name que debe recibir una url y devolver el nombre del archivo.  

extract_name <- function(url){
  file_names <- str_extract(url, "[^/]+\\.csv")
  return(file_names)
}               


#1.2
#Crear una función llamada download_esi_data para descargar un archivo

download_esi_data <- function(url, file_name, directory) {
  
  ruta <- file.path(directory, file_name)
  
  download.file(url, destfile = ruta, method = "auto", quiet = FALSE)
  
}


#1.3 

#Función para descargar y guardar las bases en una carpeta

descargar_y_guardar <- function(url, file_name) {
  download_esi_data(url, file_name, directorio)
}


#EJERCICIO 2  
#Crear una función llamada read_esi_data que lea un archivo. 

read_esi_data <- function(directorio) {
  data <- read_csv(directorio)
  return(data)
}


#EJERCICIO 4

#4.1
#Función para calcular el promedio
calcular_promedio <- function(base) {
  base2 <- base[base$ocup_ref == 1, ]
  promedio <- mean(base2$ing_t_p * base2$fact_cal_esi, na.rm = TRUE)
  return(data.frame(promedio = promedio))
}

#4.3
#Función para calcular el promedio
calcular_promedio_dt <- function(tabla) {
  setDT(tabla)     
  tabla_filtrada <- tabla[ocup_ref == 1] 
  return(mean(tabla_filtrada$ing_t_p*tabla_filtrada$fact_cal_esi))
}


