# Vector de paquetes a cargar
packages_to_load <- c("Hmisc", type = "binary","quantreg","stringr", "dplyr", "tidyverse", "data.table", "purrr", "microbenchmark", "readr","curl")

# Instalar y cargar paquetes si no están instalados
for (package in packages_to_load) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
  library(package, character.only = TRUE)
}

# Fuente del archivo functions.R si es necesario
if (!file.exists("functions.R")) {
  cat("Archivo functions.R no encontrado. Asegúrate de que el archivo existe.")
} else {
  source("functions.R")
}

# Directorio donde guardar los archivos
data_directory <- "data"

# Crear carpeta data si no existe
if (!dir.exists(data_directory)) {
  dir.create(data_directory)
}

# URLs de los archivos
urls <- c(
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)

# Descargar todos los archivos y guardarlos en el directorio

lapply(urls, function(url) {
  file_name <- extract_name(url)
  full_path <- file.path(data_directory, file_name)
  
  tryCatch({
    curl_download(url, destfile = full_path)
    cat("Archivo descargado en la carpeta:", full_path, "\n")
  }, error = function(e) {
    cat("Error al descargar:", url, "\n")
  })
})

#Ejercicio 2: leer archivos#


#Cargar los archivos
archivos <- c(
  "esi-2016-personas.csv",
  "esi-2017-personas.csv",
  "esi-2018-personas.csv",
  "esi-2019-personas.csv",
  "esi-2020-personas.csv",
  "esi-2021-personas.csv"
)

data_list <- map(archivos, read_esi_data)

version_table <- data_list %>%
  map(~ summarise(., version = paste0("esi_", unique(.$ano_trimestre)), 
                  n_personas = n_distinct(.data$idrph), 
                  n_hogares = n_distinct(.data$id_identificacion))) %>%
  bind_rows()

#Tabla.2

data_one_per_hogar <- lapply(data_list, function(df) df %>% group_by(id_identificacion) %>% slice(1))


expansion_stats <- data_list %>%
  map_dfr(~ group_by(., version = paste0("esi_", unique(.$ano_trimestre))) %>%
            slice(1) %>%
            summarise(min = min(fact_cal_esi),
                      max = max(fact_cal_esi),
                      mean = mean(fact_cal_esi),
                      median = median(fact_cal_esi),
                      p10 = quantile(fact_cal_esi, probs = 0.1),
                      p90 = quantile(fact_cal_esi, probs = 0.9))) %>%
  bind_rows()

#Tabla 3

estratos_por_conglomerado <- data_list %>%
  map(~ group_by(., version = paste0("esi_", unique(.$ano_trimestre)), conglomerado) %>%
        summarise(n_estratos = n_distinct(estrato)))

estratos_por_conglomerado <- bind_rows(estratos_por_conglomerado)

estratos_unicos <- estratos_por_conglomerado %>%
  group_by(version) %>%
  summarise(n_estratos_por_unidad_primaria = sum(n_estratos == 1))

#Ejercicio 4: mejorando el código#
#install.packages("quantreg")



estadisticas_ingresos <- data_list %>%
  map(~ {
    version <- paste0("esi_", unique(.$ano_trimestre))
    filtered_data <- filter(., ocup_ref == 1)  # Filtrar solo los ocupados que tienen ingresos
    summarise(filtered_data, 
              version = version,
              min = min(ing_t_p * fact_cal_esi),
              max = max(ing_t_p * fact_cal_esi),
              mean = weighted.mean(ing_t_p, w = fact_cal_esi),
              median = wtd.quantile(ing_t_p, weights = fact_cal_esi, probs = 0.5),
              p10 = wtd.quantile(ing_t_p, weights = fact_cal_esi, probs = 0.1),
              p90 = wtd.quantile(ing_t_p, weights = fact_cal_esi, probs = 0.9))
  })

estadisticas_ingresos <- bind_rows(estadisticas_ingresos)

ejercicio_4.1 <- map2_dfr(data_list, archivos, ~calculo_4.1(.x))

datos1 <- lapply(data_list, as.data.frame)
combinar_data <- rbindlist(datos1, use.names = TRUE, fill = TRUE)
filtros_data <- combinar_data[ocup_ref == 1]
filtros_data <- filtros_data[, Version := paste0("esi_", ano_trimestre)]

ejercicio_4.2 <- filtros_data %>%
  group_by(Version) %>%
  summarise(Media = weighted.mean(ing_t_p, w = fact_cal_esi))

ejercicio_4.3 <- lapply(data_list, calculo_4.3)
resultados_combinados <- do.call(rbind, ejercicio_4.3)
print(resultados_combinados)

datos <- lapply(data_list, as.data.table)
combined_data <- rbindlist(datos, use.names = TRUE, fill = TRUE)
filtered_data <- combined_data[ocup_ref == 1]
ejercicio_4.4 <- filtered_data[, .(Media = weighted.mean(ing_t_p, w = fact_cal_esi)), by = .(Version = paste0("esi_", ano_trimestre))]