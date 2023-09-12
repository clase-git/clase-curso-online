<<<<<<< HEAD

#########################################
############### FUNCIONES ###############
#########################################

#########################################
#### EJERCICIO 1: DESCARGAR ARCHIVOS ####
#########################################

############# PARTE 1.1 #################

# Archivo "functions.R"

extract_name <- function(urls) {
  year <- sub(".*/(\\d{4})/.*", "\\1", urls) #Regex para extraer el año
  result <- paste0("esi-", year, "-personas.csv") #Combinamos "año" con nombre del archivo
  result <- iconv(result, to = "UTF-8", sub = "byte") # Convierte a UTF-8, tuve problemas con guiones largos
  return(result)
}

download_esi_data <- function(urls, file_names, directory) {
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  
  for (i in 1:length(urls)) {
    full_path <- file.path(directory, file_names[i])
    curl_download(urls[i], destfile = full_path)
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

#Archivos a procesar 

archivos <- c("data/esi-2016-personas.csv",
              "data/esi-2017-personas.csv",  
              "data/esi-2018-personas.csv",
              "data/esi-2019-personas.csv",
              "data/esi-2020-personas.csv",
              "data/esi-2021-personas.csv")


#Cargar archivos especificando tipos de columna
datos <- map(archivos, ~ read_csv(.x, col_types = cols(.default = "d"))) # "d"=tipo numérico

#Promedio y comparar tiempo de ejecución

benchmark_result <- microbenchmark(
  purrr = calcular_promedio(datos)$purrr, 
  apilado = calcular_promedio(datos)$apilado, 
  purrr_dt = calcular_promedio(datos)$purrr_dt, 
  dt = calcular_promedio(datos)$dt, 
  unit = "ms", 
=======

#########################################
############### FUNCIONES ###############
#########################################

#########################################
#### EJERCICIO 1: DESCARGAR ARCHIVOS ####
#########################################

############# PARTE 1.1 #################

# Archivo "functions.R"

extract_name <- function(urls) {
  year <- sub(".*/(\\d{4})/.*", "\\1", urls) #Regex para extraer el año
  result <- paste0("esi-", year, "-personas.csv") #Combinamos "año" con nombre del archivo
  result <- iconv(result, to = "UTF-8", sub = "byte") # Convierte a UTF-8, tuve problemas con guiones largos
  return(result)
}

download_esi_data <- function(urls, file_names, directory) {
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  
  for (i in 1:length(urls)) {
    full_path <- file.path(directory, file_names[i])
    curl_download(urls[i], destfile = full_path)
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

#Archivos a procesar 

archivos <- c("data/esi-2016-personas.csv",
              "data/esi-2017-personas.csv",  
              "data/esi-2018-personas.csv",
              "data/esi-2019-personas.csv",
              "data/esi-2020-personas.csv",
              "data/esi-2021-personas.csv")


#Cargar archivos especificando tipos de columna
datos <- map(archivos, ~ read_csv(.x, col_types = cols(.default = "d"))) # "d"=tipo numérico

#Promedio y comparar tiempo de ejecución

benchmark_result <- microbenchmark(
  purrr = calcular_promedio(datos)$purrr, 
  apilado = calcular_promedio(datos)$apilado, 
  purrr_dt = calcular_promedio(datos)$purrr_dt, 
  dt = calcular_promedio(datos)$dt, 
  unit = "ms", 
>>>>>>> b722f1f0b28a736621cb7062c4cd3d344c1825b7
  times = 5)