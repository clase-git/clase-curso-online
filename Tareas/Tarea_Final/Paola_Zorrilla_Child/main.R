
if(!require("stringr")){
  install.packages("stringr")
  library(dplyr)
} 

if(!require("dplyr")){
  install.packages("dplyr")
  library(dplyr)
} 

if(!require("tidyverse")){
  install.packages("tidyverse")
  library(tidyverse)
} 

if(!require("data.table")){
  install.packages("data.table")
  library(data.table)
} 

if(!require("purrr")){
  install.packages("purrr")
  library(purrr)
} 

if(!require("microbenchmark")){
  install.packages("microbenchmark")
  library(microbenchmark)
} 

if(!require("readr")){
  install.packages("readr")
  library(microbenchmark)
} 

source("functions.R")

#EJERCICIO 1----

#Directorio donde guardar los archivos
data_directory <- "data"

#Crea carpeta data
if (!dir.exists(data_directory)) {
  dir.create(data_directory)
}

#URLs de los archivos
urls <- c(
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)

#Descargar todos los archivos
walk2(urls, extract_name(urls), ~download_esi_data(.x, .y, data_directory))

#EJERCICIO 2 ----

#Ahora cargaremos los archivos en la sesión. Recuerda que no necesariamente todos los archivos tienen el mismo separador.
#Crea una función llamada read_esi_data que lea un archivo. La función recibe como argumento la ruta del archivo (ej: data/esi-2018—personas.csv). read_esi_data debe ser capaz de reconocer el tipo de separador y leer el archivo correctamente en todos los casos. Para lograr esto existen varios caminos.
#Dependiendo de las versiones de tus dependencias, es posible que tengas dificultades para usar readr::read_csv. Si tienes problemas, considera utilizar otras funciones para leer archivos.


#Cargar los archivos
archivos <- c(
  "data/esi-2016.csv",
  "data/esi-2017.csv",
  "data/esi-2018.csv",
  "data/esi-2019.csv",
  "data/esi-2020.csv",
  "data/esi-2021.csv"
)

# Cargar los datos en una lista
data_list <- map(archivos, read_esi_data)

#EJERCICIO 3 ----

#ITEM 1: Tabla que contenga 3 columnas: version, n_personas (idrph) y n_hogares (id_identificacion). En la columna version debes usar la siguiente estructura: esi_{año}. Ejemplo: esi_2017
# Crear una tabla con version, n_personas e n_hogares
version_table <- data_list %>%
  map(~ summarise(., version = paste0("esi_", unique(.$ano_trimestre)), 
                  n_personas = n_distinct(.data$idrph), 
                  n_hogares = n_distinct(.data$id_identificacion))) %>%
  bind_rows()

# Visualizar la tabla resultante
print(version_table)


#ITEM 2: Tabla que contenga mínimo, máximo, media, mediana, p10 y p90 del factor de expansión (fact_cal_esi) para cada versión. Debes considerar una fila por hogar (id_identificacion) e incluir la columna version ¿Se observan algunos pesos de muestreo atípicos?

#Dejar un registro por hogar
data_one_per_hogar <- lapply(data_list, function(df) df %>% group_by(id_identificacion) %>% slice(1))


expansion_stats <- data_one_per_hogar %>%
  map(~ group_by(., version = paste0("esi_", unique(.$ano_trimestre))) %>%
        summarise(min = min(fact_cal_esi),
                  max = max(fact_cal_esi),
                  mean = mean(fact_cal_esi),
                  median = median(fact_cal_esi),
                  p10 = quantile(fact_cal_esi, probs = 0.1),
                  p90 = quantile(fact_cal_esi, probs = 0.9))) %>%
  bind_rows()

# Visualizar la tabla resultante
print(expansion_stats)



#ITEM 3: Tabla que contenga el número de estratos (estrato) con una sola unidad primaria de muestro (conglomerado). Debes incluir la columna version. 

estratos_por_conglomerado <- data_list %>%
  map(~ group_by(., version = paste0("esi_", unique(.$ano_trimestre)), conglomerado) %>%
        summarise(n_estratos = n_distinct(estrato)))

estratos_por_conglomerado <- bind_rows(estratos_por_conglomerado)

estratos_unicos <- estratos_por_conglomerado %>%
  group_by(version) %>%
  summarise(n_estratos_por_unidad_primaria = sum(n_estratos == 1))

print(estratos_unicos)



#ITEM 4: Tabla que contenga mínimo, máximo, media, mediana, p10 y p90 de los ingresos del trabajo principal (ing_t_p) para cada versión. Esta tabla debe ser construida a nivel persona, utilizando el factor de expansión (fact_cal_esi).


# Calcular estadísticas de ingresos por versión


estadisticas_ingresos <- data_list %>%
  map(~ {
    version <- paste0("esi_", unique(.$ano_trimestre))
    filtered_data <- filter(., ocup_ref==1)  # Filtrar solo los ocupados que tienen ingresos
    summarise(filtered_data, 
              version = version,
              min = min(ing_t_p * fact_cal_esi),
              max = max(ing_t_p * fact_cal_esi),
              mean = weighted.mean(ing_t_p, w = fact_cal_esi),
              median = weighted_percentile(ing_t_p, w = fact_cal_esi, p = 0.5),
              p10 = weighted_percentile(ing_t_p, w = fact_cal_esi, p = 0.1),
              p90 = weighted_percentile(ing_t_p, w = fact_cal_esi, p = 0.9))
  })

# Combinar los resultados en una sola tabla
estadisticas_ingresos <- bind_rows(estadisticas_ingresos)

# Visualizar la tabla resultante
print(estadisticas_ingresos)




#EJERCICIO 4 ----

#ITEM 1.Lista de tablas: calcular promedio con herramientas de purrr

ejercicio_4.1 <- map2_dfr(data_list, archivos, ~calculo_4.1(.x))
print(ejercicio_4.1)


#2. Tablas apiladas: calcular promedio con group_by() %>% summarise() (apila una tabla sobre otra en un dataframe)


datos1 <- lapply(data_list, as.data.frame)
combinar_data <- rbindlist(datos1, use.names = TRUE, fill = TRUE)
filtros_data <- combinar_data[ocup_ref == 1]
filtros_data <- filtros_data[, Version := paste0("esi_", ano_trimestre)]

ejercicio_4.2 <- filtros_data %>%
  group_by(Version) %>%
  summarise(Media = weighted.mean(ing_t_p, w = fact_cal_esi))

print(ejercicio_4.2)



#3 Lista de tablas: calcular promedio con herramientas de purrr, utilizando una función creada por ti, que utilice data.table.

ejercicio_4.3 <- lapply(data_list, calculo_4.3)
resultados_combinados <- do.call(rbind, ejercicio_4.3)
print(resultados_combinados)



#4 Tablas apiladas: calcular promedio con data.table.

datos <- lapply(data_list, as.data.table)
combined_data <- rbindlist(datos, use.names = TRUE, fill = TRUE)
filtered_data <- combined_data[ocup_ref == 1]
ejercicio_4.4 <- filtered_data[, .(Media = weighted.mean(ing_t_p, w = fact_cal_esi)), by = .(Version = paste0("esi_", ano_trimestre))]

print(ejercicio_4.4)


#Respuestas a preguntas

comparacion <- microbenchmark(
  ejercicio1 = ejercicio_4.1,
  ejercicio2 = ejercicio_4.2,
  ejercicio3 = ejercicio_4.3,
  ejercicio4 = ejercicio_4.4,
  times = 5)

print(comparacion)


#¿Existen diferencias importantes entre las distintas estrategias? 
#Se puede apreciar que en el ejercicic 1 y 2 donde se usa purrr (map) son más eficientes, y entre estos sin data.table lo es más.

#¿Hay alguna más eficiente que otra? 
# La más eficiente veo que es map de purrr.

#¿Usar group_by versus map hace alguna diferencia?
#Usar map es más eficiente que dplyr, si bien aca no es mucha la diferencia al aplicar con más iteraciones y calculos mas copmplejos esta diferencia se agranda.