# ++++++++++++++++++++++++++++++++++++++++++++++++++ ----------------------
# + Funciones para uso en Tarea Final -----------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++ ----------------------

# ++++++++++++++++++++++++++++++++++++++++++++++++++ ----------------------
# función que extrae nombres de archivos ESI desde url
# ++++++++++++++++++++++++++++++++++++++++++++++++++ ----------------------
extract_name <- function(url) {
  match <- regexpr("esi-(\\d{4})---(\\w+\\.csv)", url)
  file_name  <- regmatches(url, match)
  return(file_name)
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++ ----------------------
# # función que descarga bases de datos de la ESI desde url
# # ++++++++++++++++++++++++++++++++++++++++++++++++++ ----------------------
download_esi_data <- function(urls, names, directory) {
  working  <- here(directory) # Obtener la ruta absoluta del directorio de trabajo relativo
  # descargar y guardar los archivos
  walk2(urls, names, ~{
    file_path <- file.path(directory, .y)
    curl_download(.x, file_path)
  })
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++ ----------------------
# # función que carga la base de datos que recibe como argumento la ruta del archivo
# # ++++++++++++++++++++++++++++++++++++++++++++++++++ ----------------------

# utulizando función readr::read_csv
read_esi_data <- function(file_path) {
  full_path <- here(file_path)
  # Leer el archivo CSV detectando automáticamente el separador
  data <- readr::read_csv(full_path)
  return(data)
}

# generando código con posibles separadores
read_esi_data2 <- function(file_path) {
  full_path <- here(file_path)
  # Intenta adivinar el separador analizando las primeras líneas
  possible_delimiters <- c(",", ";", "\t")  # se pueden agregar otros delimitadores si es necesario
  for (delimiter in possible_delimiters) {
    data <- tryCatch(read_delim  (file_path, delim = delimiter, col_names = TRUE),
                            error = function(e) {
                              NULL
                            }
    )
    if (!is.null(data)) {
      return(data)
    }
  }
  # Si no se pudo leer con ningún delimitador, muestra un mensaje de error
  stop ("No se pudo leer el archivo con ningún delimitador conocido.")
}


# ++++++++++++++++++++++++++++++++++++++++++++++++++ ----------------------
# # función que descarga todas las bases de datos de la ESI desde carpeta data y las carga en el entorno global
# ++++++++++++++++++++++++++++++++++++++++++++++++++ ----------------------
read_all_esi <- function(folder_path) {
  working  <- here(folder_path)
  csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE) # Obtener la lista de archivos CSV en la carpeta
  # Leer y cargar los archivos CSV en el entorno global
  csv_data <- map(csv_files, ~ read.csv(.x))
  names(csv_data) <- tools::file_path_sans_ext(basename(csv_files))
  list2env(csv_data, envir = globalenv())
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++ ----------------------
# #  Función que crea Tabla que contenga 3 columnas: version, n_personas (idrph) y n_hogares (id_identificacion).
# ++++++++++++++++++++++++++++++++++++++++++++++++++ ----------------------
generarTabla <- function(data) {
  tabla <- tibble(
    version = paste0("esi_", unique(data$ano_trimestre)),
    n_personas = n_distinct(data$idrph),
    n_hogares = n_distinct(data$id_identificacion)
  )
  return(tabla)
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++ ----------------------
# #  Función para trabajar con varias bases de datos de la ESI
# ++++++++++++++++++++++++++++++++++++++++++++++++++ ----------------------

unir_bases <- function(bases) {
  seleccionar_variables <- function(df) {
    df %>%
      select(ano_trimestre, conglomerado, estrato, id_identificacion, idrph, ing_t_p, ocup_ref, fact_cal_esi)
  }
  bases_unidas <- map(bases, seleccionar_variables)
  resultado <- bind_rows(bases_unidas)
  return(resultado)
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++ ----------------------
# # Función que crea tabla que contenga mínimo, máximo, media, mediana, p10 y p90 del factor de expansión (fact_cal_esi) como entrada recibe una base de datos de la ESI
# ++++++++++++++++++++++++++++++++++++++++++++++++++ ----------------------

tabla_resultados_esi <- function(data) {
  setDT(data)  # Convertir el dataframe a data.table
  data_filtrada <- data[, .SD[1], by = id_identificacion]  # Filtrar una fila por hogar (usando id_identificacion)
  # Calcular estadísticas
  resumen <- data_filtrada[, .(Min = min(fact_cal_esi),
                               Max = max(fact_cal_esi),
                               Media = mean(fact_cal_esi),
                               Mediana = median(fact_cal_esi),
                               P10  = quantile(fact_cal_esi, 0.10),
                               P90 = quantile(fact_cal_esi, 0.90),
                               version = paste("esi_", unique(data$ano_trimestre), sep = ""))]
  return(resumen)
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++ ----------------------
# # función que crea Tabla que contenga el número de estratos (estrato) con una sola unidad primaria de muestro (conglomerado), como entrada recibe una base de datos de la ESI
# ++++++++++++++++++++++++++++++++++++++++++++++++++ ----------------------

contar_estratos <- function(data) {
  estrato_conglomerado_count  <- data[, .N, by = .(estrato, conglomerado)] # Contar el número de conglomerados por estrato
  estratos_con_unidad <- estrato_conglomerado_count[, .N, by = .(estrato)][N == 1] # Contar el número de estratos con una sola unidad primaria de muestreo
  version <- paste("esi_", unique(data$ano_trimestre), sep = "") # columna 'version'
  resultado <- data.table(num_estratos = nrow(estratos_con_unidad), version = version)# Crear la tabla de resultados
  return(resultado)
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++ ----------------------
# # función que crea Tabla que contenga mínimo, máximo, media, mediana, p10 y p90 de los ingresos del trabajo principal (ing_t_p) como entrada recibe una base de datos de la ESI
# ++++++++++++++++++++++++++++++++++++++++++++++++++ ----------------------
# para los cálculos dejé fuera a las personas que no tienen ingresos del trabajo, ing_t_p > 0, ya que si los consideraba el min, mediana y p10 daba 0

calcular_estadisticas_ing <- function(data) {
  dt  <- data.table(data)  # Creo el data.table
  dt <- dt[ing_t_p > 0]  # Filtrar las observaciones con ingresos mayores a 0
  # Calcular las estadísticas descriptivas 
  estadisticas <- dt[, .(Min = min(ing_t_p * fact_cal_esi),
                         Max = max(ing_t_p * fact_cal_esi),
                         Media = mean(ing_t_p * fact_cal_esi),
                         Mediana = median(ing_t_p * fact_cal_esi),
                         P10 = quantile(ing_t_p * fact_cal_esi, 0.1),
                         P90 = quantile(ing_t_p * fact_cal_esi, 0.9),
                         version = paste("esi_", unique(data$ano_trimestre), sep = ""))]
  return(estadisticas)
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++ ----------------------
# # funciiones para calcular el ingreso promedio con distintas estrategias, recibe como entrada una base de datos de la ESI
# ++++++++++++++++++++++++++++++++++++++++++++++++++ ----------------------

#### con purrr, lista de tablas
ingreso_medio_purrr <- function(data) {
  data %>%
    filter(ocup_ref == 1) %>%
    split(.$ano_trimestre) %>%
    map(~ sum(.x$ing_t_p * .x$fact_cal_esi) / sum(.x$fact_cal_esi))
}

# con group_by() %>% summarise(), tablas apiladas
ingreso_medio_tablas <- function(data) {
  resultados <- data %>%
    group_by(ano_trimestre) %>%
    filter(ocup_ref == 1) %>%
    summarize(ingreso_medio = sum(ing_t_p * fact_cal_esi) / sum(fact_cal_esi))
  return(resultados)
}

# con data table, lista de tablas
ingreso_medio_datatable <- function(data) {
  setDT(data)  
  data <- data[ocup_ref == 1]
  promedio_por_ano <- data[, .(ingreso_medio = sum(ing_t_p * fact_cal_esi) / sum(fact_cal_esi)), by = .(ano_trimestre)]
  lista_tablas <- split(promedio_por_ano, promedio_por_ano$ano_trimestre)
  return(lista_tablas)
}

# con data table, tablas apiladas
ingreso_medio_datatable2 <- function(data) {
  dt <- as.data.table(data)
  dt <- dt[ocup_ref == 1]
  promedio_por_ano <- dt[, .(ingreso_medio = sum(ing_t_p * fact_cal_esi) / sum(fact_cal_esi)), by = .(ano_trimestre)]
  return(promedio_por_ano)
}
