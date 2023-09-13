library(stringr)
library(purrr)
library(readr)
library(dplyr)
library(microbenchmark)

extract_name <- function(url) {
  # Expresión regular para encontrar el nombre del archivo entre "/" y "?"
  name <- str_extract(url, "(?<=/)[^/]*(?=\\?)")
  return(name)
}

# Vector de URLs
urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)

#encontrar los nombres de los archivos dentro de las url
file_names <- map(urls, extract_name)

# Imprimir los nombres de los archivos
print(file_names)

if (!dir.exists("data")) {
  dir.create("data")
}
#Crea una función llamada download_esi_data para descargar un archivo
download_esi_data <- function(url, file_name, directory) {
  download.file(url, file.path(directory, file_name), mode = "wb")
}
#descarga todos los archivos en una carpeta llamada data 
walk2(urls, file_names, download_esi_data, directory = "data")


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


# Rutas de los archivos descargados
file_paths <- file.path("data", file_names)



# Leer los archivos y almacenarlos en una lista
data_list <- lapply(file_paths, read_esi_data)

# # Vector de rutas de archivos
file_paths <- c(
  "data/esi-2021---personas.csv",
  "data/esi-2020---personas.csv",
  "data/esi-2019---personas.csv",
  "data/esi-2018---personas.csv",
  "data/esi-2017---personas.csv",
  "data/esi-2016---personas.csv"
)



# Crear una lista para almacenar los datos de cada archivo
data_list <- list()



# Leer y procesar cada archivo
for (file_path in file_paths) {
  data <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)
  
  # Renombrar columnas según sea necesario
  if ("año" %in% colnames(data)) {
    data <- data %>% rename(idrph = año)
  }
  
  if ("id_identificacion" %in% colnames(data)) {
    data <- data %>% rename(id_hogares = id_identificacion)
  }
  
  if ("estrato" %in% colnames(data)) {
    data <- data %>% rename(estrato = estrato)
  }
  
  if ("conglomerado" %in% colnames(data)) {
    data <- data %>% rename(conglomerado = conglomerado)
  }
  
  # Extraer el año del nombre del archivo
  year <- as.numeric(gsub("\\D", "", file_path))
  
  # Crear la columna 'version'
  data$version <- paste0("esi_", year)
  
  data_list[[file_path]] <- data
}



# Combinar todos los datos en una única tabla
combined_data <- bind_rows(data_list)

# Resumir la tabla según 'version', 'idrph', y 'id_hogares'
tabla_final <- combined_data %>%
  group_by(version) %>%
  summarise(n_personas = n_distinct(idrph), n_hogares = n_distinct(id_hogares))

# Mostrar la tabla final
print(tabla_final)

tabla_estadisticas <- combined_data %>%
  group_by(version) %>%
  summarise(
    min_fact_cal_esi = min(fact_cal_esi, na.rm = TRUE),
    max_fact_cal_esi = max(fact_cal_esi, na.rm = TRUE),
    mean_fact_cal_esi = mean(fact_cal_esi, na.rm = TRUE),
    median_fact_cal_esi = median(fact_cal_esi, na.rm = TRUE),
    p10_fact_cal_esi = quantile(fact_cal_esi, probs = 0.1, na.rm = TRUE),
    p90_fact_cal_esi = quantile(fact_cal_esi, probs = 0.9, na.rm = TRUE)
  )



# Mostrar la tabla de estadísticas
print(tabla_estadisticas)

tabla_estratos <- combined_data %>%
  group_by(version, estrato) %>%
  summarise(n_conglomerados = n_distinct(conglomerado))


tabla_estratos <- tabla_estratos%>%
  filter(n_conglomerados == 1)


# Mostrar la tabla de estratos con una sola unidad primaria de muestreo
print(tabla_estratos)

tabla_estadisticas <- combined_data %>%
  group_by(version) %>%
  filter(ocup_ref==1)%>%
  mutate(ing_ponderado=ing_t_p*fact_cal_esi)%>%
  summarise(
    min = min(ing_ponderado,  na.rm = TRUE),
    max = max(ing_ponderado, na.rm = TRUE),
    mean= mean(ing_ponderado,na.rm = TRUE),
    median = median(ing_ponderado,  na.rm = TRUE),
    p10 = quantile(ing_ponderado,  0.10,  na.rm = TRUE),
    p90 = quantile(ing_ponderado, 0.90,  na.rm = TRUE)
  )



# Mostrar la tabla de estadísticas
print(tabla_estadisticas)


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


# Ejecuta la comparación de tiempo de ejecución usando microbenchmark