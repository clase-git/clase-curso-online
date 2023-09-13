
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

if(!require("curl")){
  install.packages("curl")
  library(curl)
} 


if(!require("here")){
  install.packages("here")
  library(here)
} 

if(!require("readr")){
  install.packages("readr")
  library(readr)
} 

if(!require("microbenchmark")){
  install.packages("microbenchmark")
  library(microbenchmark)
} 


######## EJERCICIO 1 ####################
## Carga funciones
source("Functions.R")

#Directorio donde guardar los archivos
data_directory <- "data"

#Crea carpeta data
if (!dir.exists(data_directory)) {
  dir.create(data_directory)
}


urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
) 


#Se crea vector para encontrar los nombres de los archivos dentro de las url
file_names <- map(urls, extract_name)

# Imprimir los nombres de los archivos
print(file_names)

# Descargo los archivos en la carpeta llamada "data"
download_esi_data(urls, file_names, "data")


############## EJERCICIO 2 #############
########################################

#Ahora cargaremos los archivos en la sesión.
#Recuerda que no necesariamente todos los archivos tienen el mismo separador.
#Crea una función llamada read_esi_data que lea un archivo. 
#La función recibe como argumento la ruta del archivo (ej: data/esi-2018—personas.csv). 
#read_esi_data debe ser capaz de reconocer el tipo de separador y leer el archivo correctamente en todos los casos.

# Leer todos los archivos en una lista
data_list <- map(file_names, ~read_esi_data(file.path("data", .)))


################### EJERCICIO 3 ##############
##############################################
#Tabla que contenga 3 columnas: version, n_personas (idrph) y n_hogares (id_identificacion). 
#En la columna version debes usar la siguiente estructura: esi_{año}. Ejemplo: esi_2017


# # Vector de rutas de archivos
file_paths <- c(
  "data/esi-2021.csv",
  "data/esi-2020.csv",
  "data/esi-2019.csv",
  "data/esi-2018.csv",
  "data/esi-2017.csv",
  "data/esi-2016.csv"
)


# Se crea una lista para almacenar los datos de cada archivo
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

# Convertir combined_data a data.table
setDT(combined_data)

# Tabla 1
# Resumir la tabla según 'version', 'idrph', y 'id_hogares' usando data.table
tabla_final <- combined_data[, .(n_personas = uniqueN(idrph), n_hogares = uniqueN(id_hogares)), by = .(version)]

# Mostrar la tabla final
print(tabla_final)

# Tabla 2
# Calcular estadísticas del factor de expansión por versión y por hogar
tabla_estadisticas <- combined_data[, .(min_fact_cal_esi = min(fact_cal_esi),
                                 max_fact_cal_esi = max(fact_cal_esi),
                                 mean_fact_cal_esi = mean(fact_cal_esi),
                                 median_fact_cal_esi = median(fact_cal_esi),
                                 p10_fact_cal_esi = quantile(fact_cal_esi, 0.1),
                                 p90_fact_cal_esi = quantile(fact_cal_esi, 0.9)),
                             by = .(version, id_hogares)]

# Ver la tabla de estadísticas
print(tabla_estadisticas)

## Tabla 3

#Tabla que contenga el número de estratos (estrato) con una sola unidad primaria de muestro (conglomerado). 
# Calcular el número de conglomerados por versión y estrato
tabla_estratos <- combined_data[, .(num_conglomerado = .N), by = .(version, estrato)]

# Filtrar las filas donde num_conglomerado sea igual a 1
tabla_estratos <- tabla_estratos[num_conglomerado == 1, .(num_estratos = .N), by = .(version, estrato)]

# Ver la tabla de estratos
print(tabla_estratos)

#No existen estratos donde el número de conglomerados sea 1, ya que segun el primer calculo existen estratos con 5 o más conglomerados. 

## Tabla 4 

# Calcular estadísticas de ingresos por versión y a nivel de persona

tabla_ingresos <- combined_data[ocup_ref == 1 , .(min_ingresos = min(ing_t_p*fact_cal_esi),
                                           max_ingresos = max(ing_t_p*fact_cal_esi),
                                           media_ingresos = mean(ing_t_p*fact_cal_esi),
                                           mediana_ingresos = median(ing_t_p*fact_cal_esi),
                                           p10_ingresos = quantile(ing_t_p*fact_cal_esi, 0.10),
                                           p90_ingresos = quantile(ing_t_p*fact_cal_esi, 0.90)),
                         by = version]

#Se muestran los resultados
print(tabla_ingresos)


########## EJERCICIO 4 ##############
######################################


# Ejecuta la comparación de tiempo de ejecución usando microbenchmark
mb <- microbenchmark(
  strategy1(),
  strategy2(),
  strategy3(),
  strategy4(),
  times = 5  # Número de iteraciones
)

# Muestra los resultados
print(mb)

#según el resultado obtenido es las tablas apiladas las que menos demoran en ejecutarse
