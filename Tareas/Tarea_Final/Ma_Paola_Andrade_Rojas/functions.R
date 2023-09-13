
#########################################
############### FUNCIONES ###############
#########################################

#########################################
#### EJERCICIO 1: DESCARGAR ARCHIVOS ####
#########################################

############# PARTE 1.1 #################

# Función para extraer nombres de archivo de las URLs

extract_name <- function(urls) {
  
  file_names <- gsub(".*/([^?]+)\\?.*", "\\1", urls)
  file_names <- gsub("[[:space:]\\\\/:*?\"<>|]", "_", file_names)
  file_names <- gsub("-+", "-", file_names)
  file_names <- gsub("^-|-$", "", file_names)
  file_names <- tolower(file_names)
  return(file_names)
}

# Función para descargar un solo archivo

download_esi_data <- function(url_2021, file_name, directory) {
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)# Verificar si el directorio existe, si no crearlo
  }
  
  full_path <- file.path(directory, file_name)
  GET(url_2021, write_disk(full_path, overwrite = TRUE))  # Descargar el archivo desde la URL
  
  cat("Archivo descargado en carpeta:", full_path, "\n")
}

# Definir la URL, nombre de archivo y directorio
url_2021 <- "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true"
file_name <- "esi-2021-personas.csv"
directory <- "data_esi2021"

# Descargar el archivo
download_esi_data(url_2021, file_name, directory)

# Descargar todos los archivos

directory2 <- "data_esi"
download_esi_data <- function(urls, file_names, directory) {
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE) # Verificar si el directorio existe, si no, crearlo
  }
  
  for (i in seq_along(urls)) {
    full_url <- urls[i] # Obtener la URL correspondiente a este archivo
    full_path <- file.path(directory, file_names[i]) # Construir la ruta completa para guardar el archivo
    GET(full_url, write_disk(full_path, overwrite = TRUE)) # Descargar el archivo desde la URL
    
    cat("Archivo descargado en carpeta:", full_path, "\n")
  }
}



########################################
###### EJERCICIO 2: LEER ARCHIVOS ######
########################################

### Función que lea un archivo ESI ###

read_esi_data <- function(file_path) {
  tryCatch(
    {
      data <- read_delim(file_path, delim = ",") #Leer con comas como separador
      return(data)
    },
    error = function(e) {
    
      tryCatch(
        {
          data <- read_delim(file_path, delim = ";") #Si hay error intentará con punto y coma
          return(data)
        },
        error = function(e2) {
          stop("No se pudo leer archivo. Verificar formato y separador.") #Mensaje de error para verificación
        }
      )
    }
  )
}

########################################
###### EJERCICIO 3: OBTENER DATOS ######
########################################

######## PARTE 3.1 ########

procesar_archivo <- function(archivo_path) {
  
  datos <- read_csv(archivo_path)
  archivo_nombre <- basename(archivo_path) 
  version <- sub("esi-(\\d+).*", "esi_\\1", archivo_nombre) #Extraer año del nombre archivo ESI
  
  datos_renombrados <- datos %>%
    rename(n_personas = idrph, n_hogares = id_identificacion)
  
  datos_renombrados$version <- ifelse(!is.na(version), version, "Desconocido") #Agregar columna "version" con estructura esi_{año} o "desconocido" si no puede extraer año
  
  datos_seleccionados <- datos_renombrados %>%
    select(version, n_personas, n_hogares)
  
  return(datos_seleccionados)
}


######## PARTE 3.2 ########

# Cálculo de estadísticas descriptivas

estadisticas_descriptivas <- function(archivo_path, datos) {
  datos_lectura <- read.csv(archivo_path)
  version <- str_extract(basename(archivo_path), "\\d{4}") # Extraer año archivo ESI personas
  datos_lectura$version <- version
  
  estadisticas <- datos_lectura %>%
    group_by(id_identificacion, version) %>%
    summarise(
      min_fact_cal_esi = min(fact_cal_esi, na.rm = TRUE),
      max_fact_cal_esi = max(fact_cal_esi, na.rm = TRUE),
      media_fact_cal_esi = mean(fact_cal_esi, na.rm = TRUE),
      mediana_fact_cal_esi = median(fact_cal_esi, na.rm = TRUE),
      p10_fact_cal_esi = quantile(fact_cal_esi, 0.10, na.rm = TRUE),
      p90_fact_cal_esi = quantile(fact_cal_esi, 0.90, na.rm = TRUE),
      .groups = 'drop' # Desagrupa resultados
    )
  
  return(estadisticas)
}



######## PARTE 3.3 ########

tabla_estratos <- function(archivo_path, version) {
  
  datos <- read.csv(archivo_path)
  datos$version <- version # Agregar columna "version"
  
  tabla_unidades_primarias <- datos %>% # Contar unidades primarias de muestreo (conglom.) por estrato y versión
    group_by(estrato, version) %>%
    summarise(num_unidades_primarias = n(), .groups = "drop")
  
  tabla_resultante <- tabla_unidades_primarias %>% # Filtrar estratos con unidad primaria de muestreo
    filter(num_unidades_primarias == 1)
  
  if (nrow(tabla_resultante) == 0) {
    cat("No se encontraron estratos con una sola unidad primaria de muestreo.\n")
    return(NULL)
  }
  return(tabla_resultante)
}

######## PARTE 3.4 ########

generar_tabla_estadistica <- function(archivo_path, version) {
  datos <- read.csv(archivo_path)
  datos$version <- version #agrega columna "version"
  
  tabla_estadistica <- datos %>% #estadísticas descriptivas
    summarise(
      min_ing_t_p = min(ing_t_p * fact_cal_esi),
      max_ing_t_p = max(ing_t_p * fact_cal_esi),
      media_ing_t_p = sum(ing_t_p * fact_cal_esi) / sum(fact_cal_esi),
      mediana_ing_t_p = median(ing_t_p * fact_cal_esi),
      p10_ing_t_p = quantile(ing_t_p * fact_cal_esi, 0.10),
      p90_ing_t_p = quantile(ing_t_p * fact_cal_esi, 0.90)
    )
  return(tabla_estadistica)
}

########################################
### EJERCICIO 4: MEJORANDO EL CODIGO ###
########################################

#Cálculo del promedio de ingresos

calcular_promedio <- function(datos) {
  promedio_purrr <- datos %>%
    map_dbl(~ mean(.$ing_t_p, na.rm = TRUE))
  
  promedio_apilado <- datos %>%
    bind_rows() %>%
    summarise(promedio = mean(ing_t_p, na.rm = TRUE))
  
  promedio_purrr_dt <- datos %>%
    map_dbl(~ as.data.table(.)[, .(promedio = mean(ing_t_p, na.rm = TRUE))][[1]])
  
  promedio_dt <- as.data.table(datos) %>%
    .[, .(promedio = mean(ing_t_p, na.rm = TRUE))]
  
  return(list(purrr = promedio_purrr, apilado = promedio_apilado, purrr_dt = promedio_purrr_dt, dt = promedio_dt))}

#Carga de archivos por tipos de columna
datos <- map(archivos, ~ read_csv(.x, col_types = cols(.default = "d"))) # "d"=tipo numérico

#Promedio y comparar tiempo de ejecución

benchmark_result <- microbenchmark(
  purrr = calcular_promedio(datos)$purrr, 
  apilado = calcular_promedio(datos)$apilado, 
  purrr_dt = calcular_promedio(datos)$purrr_dt, 
  dt = calcular_promedio(datos)$dt, 
  unit = "ms", 
  times = 5)