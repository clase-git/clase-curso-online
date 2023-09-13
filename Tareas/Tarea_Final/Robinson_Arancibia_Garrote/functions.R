extract_name <- function(url) {
  # Expresión regular para encontrar el nombre del archivo entre "/" y "?"
  name <- str_extract(url, "(?<=/)[^/]*(?=\\?)")
  return(name)
}

#Crea una función llamada download_esi_data para descargar un archivo
download_esi_data <- function(url, file_name, directory) {
  download.file(url, file.path(directory, file_name), mode = "wb")
}

read_esi_data <- function(file_path) {
  tryCatch({
    data <- read_csv(file_path)
    
    # Completar datos faltantes con NA
    data[is.na(data)] <- NA
    
    return(data)
  }, error = function(e) {
    # Si se produce un error, muestra un mensaje y devuelve NULL
    message("Error al leer el archivo: ", e$message)
    return(NULL)
  })
}

# Define las cuatro pruebas
prueba1 <- function() {
  # prueba1: Lista de tablas con herramientas de purrr
  result <- combined_data %>%
    group_by(version) %>%
    summarise(promedio = weighted.mean(ing_t_p, w = fact_cal_esi))
}

prueba2 <- function() {
  #prueba2: Tablas apiladas con group_by() %>% summarise()
  result <- combined_data %>%
    group_by(version) %>%
    summarise(promedio = mean(ing_t_p))
}

prueba3 <- function() {
  #prueba3: Lista de tablas con data.table
  library(data.table)
  dt <- as.data.table(combined_data)
  result <- dt[, .(promedio = weighted.mean(ing_t_p, w = fact_cal_esi)), by = version]
}



prueba4 <- function() {
  #prueba4: Tablas apiladas con data.table
  library(data.table)
  dt <- as.data.table(combined_data)
  result <- dt[, .(promedio = mean(ing_t_p)), by = version]
}
