## función extraer nombre de archivos desde una url
extract_name <- function(url) {
  nombre_archivo <- basename(url)
  nombre_archivo_sin_parametros <- sub("\\?.*$", "", nombre_archivo)
  nombre_archivo_sin_numeros <- sub("^\\d+", "", nombre_archivo_sin_parametros)
  return(nombre_archivo_sin_numeros)
}



## función descargar archivo desde url 
download_esi_data <- function(url, file_name, directory) {
  
  # Construye la ruta completa del archivo de destino
  destination_path <- file.path(directory, file_name)
  
  # Descarga el archivo desde la URL y guárdalo en el directorio de destino
  download_file(url, output = destination_path)
  
  # Verifica si la descarga fue exitosa
  if (file.exists(destination_path)) {
    cat("Descarga completada. El archivo se guardó en:", destination_path, "\n")
  } else {
    cat("La descarga falló. Verifica la URL y el directorio de destino.", "\n")
  }
}

##FUNCIÓN DE DESCARGA DE ARCHIVOS
funcion_descarga<-function(urls){
  nombre_bases<-extract_name(urls)
  directory<-file.path(getwd(), "data")
  descargar<-download_esi_data(urls,nombre_bases,directory)
}

##FUNCIÓN LEER ARCHIVOS CSV CON DISTINTOS DELIMITADORES
read_esi_data <- function(ruta_archivo) {
  library(data.table)
  
  # Intenta leer el archivo con diferentes delimitadores
  delimitadores <- c(",", ";", "\t")  # Coma, punto y coma, tabulador
  
  for (delimitador in delimitadores) {
    datos <- try(fread(ruta_archivo, sep = delimitador), silent = TRUE)
    if (!inherits(datos, "try-error")) {
      return(datos)
    }
  } 
  
  stop("No se pudo leer el archivo con ningún delimitador conocido.")
}


resumen_variable<- function(data) {
  data %>% distinct(id_identificacion, .keep_all = TRUE) %>%  
    summarize(mínimo  = min(fact_cal_esi),
              máximo  = max(fact_cal_esi),
              media  = mean(fact_cal_esi),
              mediana  = median(fact_cal_esi),
              p10  = quantile(fact_cal_esi, 0.1),
              p90  = quantile(fact_cal_esi, 0.9)
    )
}


resumen_ingresos <- function(data) {
  data %>% filter(ocup_ref == 1) %>% 
    summarize(mínimo  = min(ing_t_p),
              máximo  = max(ing_t_p),
              media  = weighted.mean(ing_t_p, fact_cal_esi),
              mediana  = quantile(ing_t_p, probs =0.5, weights = fact_cal_esi),
              p10  = quantile(ing_t_p, probs = 0.1, weights = fact_cal_esi),
              p90  = quantile(ing_t_p, probs = 0.9, weights = fact_cal_esi))
}




#4.1
#Función para calcular el promedio purrr
promedio <- function(data) {
  data1 <- data[data$ocup_ref == 1, ]
  promedio <- weighted.mean(data1$ing_t_p, data1$fact_cal_esi)
  return(data.frame(promedio = promedio))
}

#4.3
#Función para calcular el promedio data.table
promedio_3 <- function(data) {
  data1 <- as.data.table(data)
  data2 <- data1[ocup_ref == 1]
  promedio<- weighted.mean(data2$ing_t_p, data2$fact_cal_esi)
  return(data.frame(promedio = promedio))
}
