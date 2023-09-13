rm(list = ls())

# Función para extraer el nombre del archivo a partir de una URL
extract_name <- function(url) {
  # Utilizar expresión regular para extraer el año (4 dígitos) de la URL
  year <- sub(".*/(\\d{4})/.*", "\\1", url)
  
  # Construir el nombre del archivo
  file_name <- paste0("esi-", year, "-personas.csv")
  
  return(file_name)
}

# Función para descargar archivos desde URLs
download_esi_data <- function(url, file_name, directory) {
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  
  full_path <- file.path(directory, file_name)
  download.file(url, destfile = full_path)
  cat("Archivo descargado en la carpeta:", full_path, "\n")
}


read_esi_data <- function(file_path) {
  data <- read_delim(file_path) #delimitador automáticamente
  return(data)
}

#Funciones ejercicio 4

calculo_4.1 <- function(df) {
  df_filtrado <- filter(df, ocup_ref == 1)
  promedio_ponderado <- weighted.mean(df_filtrado$ing_t_p, w = df_filtrado$fact_cal_esi)
  return(data.frame(Version = paste0("esi_", unique(df$ano_trimestre)), Media = promedio_ponderado))
}


calculo_4.3 <- function(df) {
  dt <- as.data.table(df)
  dt_filtrado <- dt[ocup_ref == 1]
  promedio_ponderado <- weighted.mean(dt_filtrado$ing_t_p, w = dt_filtrado$fact_cal_esi)
  return(data.frame(Version = unique(paste0("esi_", df$ano_trimestre)), Media = promedio_ponderado))
}

