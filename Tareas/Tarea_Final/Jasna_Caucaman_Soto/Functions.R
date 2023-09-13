
# función que extrae nombres de archivos ESI desde url
#Función extract_name
extract_name <- function(url) {
  file_name <- str_extract(url, "([^/]+)$")
  file_name <- str_replace(file_name, "\\?.*$", "")
  file_name <- str_replace_all(file_name, "[-]+personas", "")
  return(file_name)
}


#Función que descarga bases de datos de la ESI desde url
download_esi_data <- function(urls, names, directory) {
  working  <- here(directory) 
  walk2(urls, names, ~{
    file_path <- file.path(directory, .y)
    curl_download(.x, file_path)
  })
}

#Función para leer archivos
read_esi_data <- function(file_path) {
  # Leer el archivo usando data.table (detectará el separador automáticamente)
  data.table::fread(file_path)
}

# Define las cuatro estrategias de cálculo
strategy1 <- function() {
  # Estrategia 1: Lista de tablas con herramientas de purrr
  result <- combined_data %>%
    group_by(version) %>%
    summarise(promedio = weighted.mean(ing_t_p, w = fact_cal_esi))
}

strategy2 <- function() {
  # Estrategia 2: Tablas apiladas con group_by() %>% summarise()
  result <- combined_data %>%
    group_by(version) %>%
    summarise(promedio = mean(ing_t_p))
}

strategy3 <- function() {
  # Estrategia 3: Lista de tablas con data.table
  library(data.table)
  dt <- as.data.table(combined_data)
  result <- dt[, .(promedio = weighted.mean(ing_t_p, w = fact_cal_esi)), by = version]
}

strategy4 <- function() {
  # Estrategia 4: Tablas apiladas con data.table
  library(data.table)
  dt <- as.data.table(combined_data)
  result <- dt[, .(promedio = mean(ing_t_p)), by = version]
}





