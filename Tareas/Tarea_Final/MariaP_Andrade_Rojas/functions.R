
#########################################
############### FUNCIONES ###############
#########################################

#########################################
#### EJERCICIO 1: DESCARGAR ARCHIVOS ####
#########################################

############# PARTE 1.1 #################

#Función extract_name para extraer nombre de archivo:

extract_name <- function(url) 
  {
  year <- sub(".*/(\\d{4})/.*", "\\1", url) #Expresión regular para extraer año
  result <- paste0("esi-", year, "—personas.csv") #Combina "año" con nombre del archivo
  return(result)
}

############# PARTE 1.2 y 1.3 ###########

#Función para descarga de archivos desde URLs

download_esi_data <- function(url, file_name, directory) {
  
  if (!dir.exists(directory)) #Verifica si existe directorio
    {
    dir.create(directory, recursive = TRUE) #Crea directorio
    }
  
  full_path <- file.path(directory, file_name) 
  curl_download(url, destfile = full_path) #Descarga y guarda archivo
  cat("Archivo descargado en carpeta:", full_path, "\n") #Mensaje de descarga una vez finalizada
  }


########################################
###### EJERCICIO 2: LEER ARCHIVOS ######
########################################

### Crear función que lea un archivo ESI ###

read_esi_data <- function(file_path) {
  tryCatch(
    {
      data <- readr::read_delim(file_path, delim = ",") #Leer con comas como separador
      return(data)
    },
    error = function(e) {
    
      tryCatch(
        {
          data <- readr::read_delim(file_path, delim = ";") #Si hay error intento con punto y coma
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
  
  #Agregar columna "version" con estructura esi_{año} o "desconocido" si no puede extraer año
  datos_renombrados$version <- ifelse(!is.na(version), version, "Desconocido")
  
  datos_seleccionados <- datos_renombrados %>%
    select(version, n_personas, n_hogares)
  
  return(datos_seleccionados)
}

######## PARTE 3.2 ########

#Crear función para calcular las estadísticas descriptivas

estadisticas_descriptivas <- function(archivo_path) {
  datos <- read.csv(archivo_path)
  version <- str_extract(basename(archivo_path), "\\d{4}") #Extraer año archivo ESI personas
  datos$version <- version
  
#Agrupar por "id_identificacion" y "version" y calcular estadísticas descriptivas para "fact_cal_esi"
  
  estadisticas <- datos %>%
    group_by(id_identificacion, version) %>%
    summarise(
      min_fact_cal_esi = min(fact_cal_esi),
      max_fact_cal_esi = max(fact_cal_esi),
      media_fact_cal_esi = mean(fact_cal_esi),
      mediana_fact_cal_esi = median(fact_cal_esi),
      p10_fact_cal_esi = quantile(fact_cal_esi, 0.10),
      p90_fact_cal_esi = quantile(fact_cal_esi, 0.90)
    )
  return(estadisticas)
}

#Listar archivos que coinciden con el patrón ESI
archivos <- list.files(path = "data/", pattern = "esi-\\d{4}—personas.csv", full.names = TRUE)

resultados <- lapply(archivos, estadisticas_descriptivas)

#Combinar resultados en un solo dataframe
tabla_df <- do.call(rbind, resultados)

#Registros únicos por "id_identificacion" y "version"
tabla_final <- unique(tabla_df, by = c("id_identificacion", "version"))
tabla_final


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

generar_tabla_estadistica <- function(archivo_path, version) 
  {
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

calcular_promedio <- function(datos) 
  {
  promedio_purrr <- datos %>%
    map_dbl(~ mean(.$ing_t_p, na.rm = TRUE))
  
  promedio_apilado <- datos %>%
    bind_rows() %>%
    summarise(promedio = mean(ing_t_p, na.rm = TRUE))
  
  promedio_purrr_dt <- datos %>%
    map_dbl(~ as.data.table(.)[, .(promedio = mean(ing_t_p, na.rm = TRUE))][[1]])
  
  promedio_dt <- as.data.table(datos) %>%
    .[, .(promedio = mean(ing_t_p, na.rm = TRUE))]
  
  return(list(purrr = promedio_purrr, apilado = promedio_apilado, purrr_dt = promedio_purrr_dt, dt = promedio_dt))
}

#Archivos a procesar 

archivos <- c("data/esi-2016—personas.csv",
              "data/esi-2017—personas.csv",  
              "data/esi-2018—personas.csv",
              "data/esi-2019—personas.csv",
              "data/esi-2020—personas.csv",
              "data/esi-2021—personas.csv")


#Cargar archivos especificando tipos de columna
datos <- map(archivos, ~ read_csv(.x, col_types = cols(.default = "d")))  # "d" = tipo numérico

#Promedio y comparar tiempo de ejecución

benchmark_result <- microbenchmark(
  purrr = calcular_promedio(datos)$purrr,
  apilado = calcular_promedio(datos)$apilado,
  purrr_dt = calcular_promedio(datos)$purrr_dt,
  dt = calcular_promedio(datos)$dt,
  unit = "ms",  #timing
  times = 5  #iteraciones
)

benchmark_result



