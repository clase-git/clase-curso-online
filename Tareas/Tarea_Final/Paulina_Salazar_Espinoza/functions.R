if(!require("stringr")) install.packages("stringr")
if(!require("purrr")) install.packages("purrr")

library(stringr)
library(purrr)

# Función para extraer el nombre del archivo de una URL
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

# Utilizar purrr para aplicar la función extract_name a cada URL
file_names <- map(urls, extract_name)

# Imprimir los nombres de los archivos
print(file_names)

if (!dir.exists("data")) {
  dir.create("data")
}

download_esi_data <- function(url, file_name, directory) {
  download.file(url, file.path(directory, file_name), mode = "wb")
}

walk2(urls, file_names, download_esi_data, directory = "data")


if (!require(readr)) {
  install.packages("readr")
  library(readr)
}

read_esi_data <- function(file_path) {
  tryCatch({
    # Intentar leer el archivo con readr::read_csv
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

# Ejemplo de uso:
file_path <- "data/esi-2017---personas.csv"
data <- read_esi_data(file_path)

# Rutas de los archivos descargados
file_paths <- file.path("data", file_names)

# Leer los archivos y almacenarlos en una lista
data_list <- lapply(file_paths, read_esi_data)

# Combinar los datos en un solo DataFrame
combined_data <- do.call(rbind, data_list)


if (!require(dplyr)) {
  install.packages("dplyr")
 }
library(dplyr)
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


# Calcular estadísticas descriptivas por versión utilizando el factor de expansión
tabla_estadisticas <- combined_data %>%
  group_by(version) %>%
  summarise(
    min_ing_t_p = weighted.mean(ing_t_p, fact_cal_esi, na.rm = TRUE),
    max_ing_t_p = weighted.mean(ing_t_p, fact_cal_esi, na.rm = TRUE),
    mean_ing_t_p = weighted.mean(ing_t_p, fact_cal_esi, na.rm = TRUE),
    #median_ing_t_p = weighted.median(ing_t_p, fact_cal_esi, na.rm = TRUE),
    #p10_ing_t_p = weighted.quantile(ing_t_p, probs = 0.10, fact_cal_esi, na.rm = TRUE),
    #p90_ing_t_p = weighted.quantile(ing_t_p, probs = 0.90, fact_cal_esi, na.rm = TRUE)
  )

# Mostrar la tabla de estadísticas
print(tabla_estadisticas)





