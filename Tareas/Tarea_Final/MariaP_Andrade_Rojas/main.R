####################################################################
####### Determinar paquetes a instalar si no están en equipo #######
####################################################################

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

#Establecer ruta relativa al directorio de trabajo
script_dir <- here()

#Carga "functions.R"
source(file.path(script_dir, "functions.R"))

##### EVALUACIÓN EJERCICIO 1: descargar archivos #####

##### 1.1 #####

urls <- c(
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
  "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)

file_names <- map_chr(urls, extract_name)
file_names


##### 1.2 #####

#Evaluación con "esi-2020—personas.csv" como ejemplo

url <- "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=d03ae552_4&download=true"
file_name <- "esi-2020—personas.csv"
directory <- "esi-2020—personas"

download_esi_data(url, file_name, directory)


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
  "esi-2021—personas.csv",
  "esi-2020—personas.csv",
  "esi-2019—personas.csv",
  "esi-2018—personas.csv",
  "esi-2017—personas.csv",
  "esi-2016—personas.csv")

directory <- "data_esi" #Genera carpeta "data_esi" en directorio

map2(urls, file_names, ~download_esi_data(.x, .y, directory)) #Descarga todos los archivos en "data_esi"


##### EVALUACIÓN EJERCICIO 2: lectura de un archivo #####

#Como ejemplo se usa "esi-2017—personas.csv"

file.exists("data_esi/esi-2017—personas.csv")
file_path <- "data_esi/esi-2017—personas.csv"  
data2017 <- read_esi_data(file_path)
data2017

##### EVALUACIÓN EJERCICIO 3: obtener datos mediante tablas #####

##### 3.1 #####

#Como ejemplo se usa "esi-2017—personas.csv"

archivo_path <- "data_esi/esi-2017—personas.csv" 
tabla_procesada <- procesar_archivo(archivo_path)
tabla_procesada

##### 3.2 #####

# Resumen
summary(tabla_final$media_fact_cal_esi)

# Diagrama de caja y gráfico de densidad
boxplot(tabla_final$media_fact_cal_esi)
plot(density(tabla_final$media_fact_cal_esi), main = "Densidad de fact_cal_esi")

# Explora valores atípicos
outliers <- grubbs.test(tabla_final$media_fact_cal_esi)$out
print(outliers)

# Explora resultado NULL: calculo de cuartiles (Q1, Q2, Q3) y percentiles (P10, P90)
cuartiles <- quantile(tabla_final$media_fact_cal_esi, probs = c(0.25, 0.50, 0.75))
percentiles <- quantile(tabla_final$media_fact_cal_esi, probs = c(0.10, 0.90))
cuartiles
percentiles

# ¿Se observan algunos pesos de muestreo atípicos?
# Aunque por cálculos utilizando funciones de R no existirían outliers (NULL), 
# según ambas gráficas y cálculo de cuartiles y percentiles sí se observan algunos pesos atípicos, 
# aunque en baja proporción respecto del total.


##### 3.3 #####

# Evaluación con esi-2017—personas.csv

archivo_path <- "data_esi/esi-2017—personas.csv"
version <- "esi_2017" 
resultado <- tabla_estratos(archivo_path, version)

if (!is.null(resultado)) {
  print(resultado)
}

# Objeto "resultado" = NULL, que coincide con resultado al ejecutar la función en script "functions"
# donde mensaje de salida es "No se encontraron estratos con una sola unidad primaria de muestreo".

##### 3.4 #####

# Evaluación con esi-2017—personas.csv

archivo_path <- "data_esi/esi-2017—personas.csv" 
version <- "esi_2017" 
resultado2 <- generar_tabla_estadistica(archivo_path, version)
resultado2

##### EVALUACIÓN EJERCICIO 4: obtener datos mediante tablas #####

# Obtener el método más rápido
metodo_mas_eficiente <- rownames(summary(benchmark_result))[which.min(summary(benchmark_result)$mean)]
cat("El método más eficiente es:", metodo_mas_eficiente, "\n")

#¿Existen diferencias importantes entre las distintas estrategias? 
#¿Hay alguna más eficiente que otra? ¿Usar group_by versus map hace alguna diferencia?



