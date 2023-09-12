

####### Determinar paquetes a instalar si no están en equipo #######

paquetes <- c("here", "purrr", "stringr", "dplyr", "readr", "curl", "outliers", "microbenchmark", "data.table")

#Verificar si los paquetes están disponibles en el equipo de trabajo
paquetes_a_instalar <- paquetes[!sapply(paquetes, requireNamespace, quietly = TRUE)]

#Instalar los paquetes faltantes
if (length(paquetes_a_instalar) > 0) {
  install.packages(paquetes_a_instalar)
}

#Cargar paquetes
library(here)
library(purrr)
library(stringr)
library(dplyr)
library(readr)
library(curl)
library(outliers)
library(microbenchmark)
library(data.table)

setwd(here())
getwd()
script_dir <- here()
#source(file.path(script_dir, "functions.R"))
source("functions.R")

##### EJERCICIO 1: descargar archivos #####

##### 1.1 #####

urls <- c(
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true")

file_names <- map_chr(urls, extract_name)
file_names


##### 1.2 #####

#Descarga de un solo archivo "esi-2016-personas.csv"

directory <- "data_esi2016"

file_names <- c("esi-2016-personas.csv")

download_esi_data(urls[1], file_names[1], directory)


##### 1.3 #####

urls <- c(
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)

file_names <- c(
  "esi-2016-personas.csv",
  "esi-2017-personas.csv",
  "esi-2018-personas.csv",
  "esi-2019-personas.csv",
  "esi-2020-personas.csv",
  "esi-2021-personas.csv")

directory <- "data_esi" #Genera carpeta "data_esi" en directorio

map2(urls, file_names, ~download_esi_data(.x, .y, directory)) #Descarga todos los archivos en "data_esi"


##### EVALUACIÓN EJERCICIO 2: lectura de un archivo #####

#Como ejemplo se usa "esi-2018—personas.csv"

file.exists("data_esi/esi-2018-personas.csv")
file_path <- "data_esi/esi-2018-personas.csv"  
data2018 <- read_esi_data(file_path)
data2018

##### EVALUACIÓN EJERCICIO 3: obtener datos mediante tablas #####

##### 3.1 #####

#Como ejemplo se usa "esi-2017—personas.csv"

archivo_path <- "data_esi/esi-2017-personas.csv" 
tabla_procesada <- procesar_archivo(archivo_path)
tabla_procesada

##### 3.2 #####

#Listar archivos que coinciden con el patrón ESI
archivos <- list.files(path = "data/", pattern = "esi-\\d{4}-personas.csv", full.names = TRUE)

resultados <- lapply(archivos, estadisticas_descriptivas)

#Combinar resultados en un solo dataframe
tabla_df <- do.call(rbind, resultados)

#Registros únicos por "id_identificacion" y "version"
tabla_final <- unique(tabla_df, by = c("id_identificacion", "version"))
tabla_final

summary(tabla_final$media_fact_cal_esi)

# Diagrama de caja
boxplot(tabla_final$media_fact_cal_esi)

# Tabla_final sin valores faltantes en variable "media_fact_cal_esi"
tabla_final_sin_na <- na.omit(tabla_final)

# Gráfica de densidad para explorar lo obtenido por caja
plot(density(tabla_final_sin_na$media_fact_cal_esi), main = "Densidad de fact_cal_esi")

# Explora valores atípicos
outliers <- grubbs.test(tabla_final$media_fact_cal_esi)$out
print(outliers)

# Explora resultado NULL: calculo de cuartiles (Q1, Q2, Q3) y percentiles (P10, P90)
cuartiles <- quantile(tabla_final$media_fact_cal_esi, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
percentiles <- quantile(tabla_final$media_fact_cal_esi, probs = c(0.10, 0.90), na.rm = TRUE)
cuartiles
percentiles

# ¿Se observan algunos pesos de muestreo atípicos?
# Respuesta: aunque por cálculos utilizando funciones de R no existirían outliers (NULL), según ambas gráficas
# (caja y densidad), cálculo de cuartiles y percentiles, sí se observan algunos pesos atípicos, aunque en baja proporción versus el total.


##### 3.3 #####

# Evaluación con esi-2017—personas.csv

archivo_path <- "data_esi/esi-2017-personas.csv"
version <- "esi_2017" 
resultado <- tabla_estratos(archivo_path, version)

if (!is.null(resultado)) {
  print(resultado)
}

# Objeto "resultado" = NULL, esto coincide con resultado al ejecutar la función en script "functions" donde 
# mensaje de salida es "No se encontraron estratos con una sola unidad primaria de muestreo".

##### 3.4 #####

# Evaluación con esi-2017—personas.csv

archivo_path <- "data_esi/esi-2017-personas.csv" 
version <- "esi_2017" 
resultado2 <- generar_tabla_estadistica(archivo_path, version)
resultado2

##### EVALUACIÓN EJERCICIO 4: obtener datos mediante tablas #####

# Obtener el método más rápido
metodo_mas_eficiente <- rownames(summary(benchmark_result))[which.min(summary(benchmark_result)$mean)]
cat("El método más eficiente es:", metodo_mas_eficiente, "\n")

print(benchmark_result)

#¿Existen diferencias importantes entre las distintas estrategias? 
# Según resultados obtenidos vía benchmark, se observan diferencias importantes conforme al equipo utilizado y la versión de R instalada. 

#¿Hay alguna más eficiente que otra? 
#El método 1 (lista de tablas + purrr) resultó ser el más eficiente en notebook personal utilizado (R 4.3.1) para realizar
#la tarea final. Por otro lado, al evaluar mismos script en computador institucional (equipo de escritorio, más antiguo)
#fue el método 4 (tablas apiladas + data.table) resultó más eficiente (R 4.1.3). 

#¿Usar group_by versus map hace alguna diferencia?
#Se observa que la elección entre herramientas group_by y/o map dependerá de la estructura de los datos, operaciones, capacidades del equipo y versión de R. 
#En mi experiencia, si tenemos un set de datos grande y necesitamos realizar operaciones en grupos "group_by" (como summarise) debería ser más eficiente.
#Por otra parte, si tenemos múltiples dataframes independientes y no de gran tamaño, y queremos aplicar la misma operación a cada uno, "map" debería funcionar mejor.
#Según lo observado al evaluar los 4 métodos, la eficiencia real debe determinarse con pruebas para cada caso específico de complejidad en 
#las operaciones a realizar y equipo disponible.
