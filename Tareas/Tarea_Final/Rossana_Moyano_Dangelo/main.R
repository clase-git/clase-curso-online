## Librerias utilizadas
if(!require("dplyr")) install.packages("dplyr")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("purrr")) install.packages("purrr")
if(!require("curl")) install.packages("curl")
if(!require("here")) install.packages("here")
if(!require("data.table")) install.packages("data.table")
if(!require("curl")) install.packages("curl")

library(curl)
library(data.table)
library(here)
library(curl)
library(purrr)
library(dplyr)
library(tidyverse)

# ++++++++++++++++++++++++++++++++++++++++++++++++++ -------
# + # Ejercicio 1:descargar archivos-----------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++ -------

urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
) 

# Carga de funciones ------------
source("functions.R")

# crea un vector llamado file_names que almacene la porción de las url que contiene el nombre de los archivos que luego descargaremos
file_names <- map(urls, extract_name)

# Descargo los archivos en la carpeta llamada "data", para esto la carpeta data debe estar creada en el proyecto previamente
download_esi_data(urls, file_names, "data")

# ++++++++++++++++++++++++++++++++++++++++++++++++++ 
# + Ejercicio 2: leer archivos-----------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++ 

# Se cargan los archivos  en el entorno global, para este ejercicio creé dos funciones una con read.csv y otra buscando posibles delimitadores, 
# sin embargo esto implica cargar cada archivo uno por uno indicandole la ruta, por lo que creee otra función  que el argumento a pasar es el nombre de la carpeta 
# donde están guardadas las bases de datos en nuestro proyecto, que para este ejercicio es "data" y se descargan todas las bases

# uso función para cargar una a una las bases
ESI_2016 <- read_esi_data("data/esi-2016---personas.csv")  # uso primera función
ESI_2017 <- read_esi_data2("data/esi-2017---personas.csv")  # uso segunda función

# para uso de función que carga todas las bases de datos que están en la carpeta data 
read_all_esi("data")


# ++++++++++++++++++++++++++++++++++++++++++++++++++ 
# + Ejercicio 3: obtener datos-----------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++

# 1. Tabla que contenga 3 columnas: version, n_personas (idrph) y n_hogares (id_identificacion). 

# ejemplo de uso
tabla2016 <- generarTabla(ESI_2016)

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Para desarrollar lo solictado más adelante lo más conveniete es unir las bases de datos, para esto primero voy a dejarlas en una lista, tengo dos opciones para esto

# 1.leer todas las bases y dejarlas en una lista usando la función read_esi_data creada anteriormente, esto lee directamente las bases de dónde están guardadas, no se necesitan cargar 
data_folder <- "data"  #  indica la carpeta donde están las bases dentro del proyecto
file_list <- list.files(path = data_folder, pattern = "*.csv", full.names = TRUE)
read_all_esi_data <- function(file_path) {
  read_esi_data (file_path)
}
esi_data_list <- map(file_list, read_all_esi_data) 

# 2. unir todas las bases en una lista de la forma tradicional, para esto tengo que tener primero todas las bases cargadas
esi_data_list <- list(`esi-2016---personas`, `esi-2017---personas`, `esi-2018---personas`, `esi-2019---personas`, `esi-2020---personas`, `esi-2021---personas`)

# unir las bases en un solo data frame para esto se creó una función llamada unir_bases
base_final_ESI <- unir_bases(esi_data_list) 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 2. Tabla que contenga mínimo, máximo, media, mediana, p10 y p90 del factor de expansión (fact_cal_esi) para cada 
# versión. Debes considerar una fila por hogar (id_identificacion) e incluir la columna version 
# ¿Se observan algunos pesos de muestreo atípicos?

# Generar tabla que muestre información para cada versión ESI utilizando la función creada tabla_resultados_esi
resultados_por_ano <- base_final_ESI %>%
  split(.$ano_trimestre) %>%
  map(tabla_resultados_esi) %>%
  bind_rows (.id = "ano_trimestre")


# 3. Tabla que contenga el número de estratos (estrato) con una sola unidad primaria de muestro (conglomerado). Debes incluir la columna version.

# Generar tabla que muestre información para cada versión ESI utilizando la función creada contar_estratos
tabla_contar_estratos <- base_final_ESI %>%
  split(.$ano_trimestre) %>%
  map(contar_estratos) %>%
  bind_rows (.id = "ano_trimestre")


# 4. Tabla que contenga mínimo, máximo, media, mediana, p10 y p90 de los ingresos del trabajo principal (ing_t_p) 
# para cada versión. Esta tabla debe ser construida a nivel persona, utilizando el factor de expansión (fact_cal_esi).

# Genero tabla que muestre información para cada versión ESI utilizando la función creada calcular_estadisticas_ing
resultados_ing_ano <- base_final_ESI %>%
  split(.$ano_trimestre) %>%
  map(calcular_estadisticas_ing) %>%
  bind_rows (.id = "ano_trimestre")

# ++++++++++++++++++++++++++++++++++++++++++++++++++ 
# + Ejercicio 4: mejorando el código  --------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++

if(!require("microbenchmark")) install.packages("microbenchmark")
library(microbenchmark)
# comparar el tiempo de ejecución de algunas estrategias. Utiliza el paquete microbenchmark
# Calcula el promedio de ingresos en las tablas de la ESI (ing_t_p) mediante las siguientes estrategias:

# ++++++++++++++++++++++++++++++++++++++++++++++++++ 
# 1. Lista de tablas: calcular promedio con herramientas de purrr (como en el ejercicio anterior)

results_purr <- microbenchmark(
  resultados_con_purr <- ingreso_medio_purrr(base_final_ESI),
  times = 5)

# ++++++++++++++++++++++++++++++++++++++++++++++++++ 
# 2. Tablas apiladas: calcular promedio con group_by() %>% summarise() (apila una tabla sobre otra en un dataframe)

results_tablas <- microbenchmark(
  calcular_ingreso_medio_tablas <- ingreso_medio_tablas(base_final_ESI),
  times = 5)

# ++++++++++++++++++++++++++++++++++++++++++++++++++ 
# 3. Lista de tablas: calcular promedio con herramientas de purrr, utilizando una función creada por ti, que utilice data.table.

results_datatable <- microbenchmark(
  calcular_ingreso_medio_datatable <- ingreso_medio_datatable(base_final_ESI),
  times = 5)

# ++++++++++++++++++++++++++++++++++++++++++++++++++ 
# 4. Tablas apiladas: calcular promedio con data.table.

results_datatable <- microbenchmark(
  calcular_ingreso_medio_datatable2 <- ingreso_medio_datatable2(base_final_ESI),
  times = 5)


####### ver todas las iteraciones juntas
results_resumen <- microbenchmark(
  resultados_con_purr <- ingreso_medio_purrr(base_final_ESI),
  calcular_ingreso_medio_tablas <- ingreso_medio_tablas(base_final_ESI),
  calcular_ingreso_medio_datatable <- ingreso_medio_datatable(base_final_ESI),
  calcular_ingreso_medio_datatable2 <- ingreso_medio_datatable2(base_final_ESI),
  times = 5)

# Resultados obtenidos con distintas estrategias
# ++++++++++++++++++++++++++++++++++++++++++++++++++ 

# > results_resumen
# Unit: milliseconds
# expr                                                                              min      lq       mean     median      uq     max      neval
# resultados_con_purr <- ingreso_medio_purrr(base_final_ESI)                        89.7355 94.0440 102.26972 96.5398  113.6056 117.4237     5
# calcular_ingreso_medio_tablas <- ingreso_medio_tablas(base_final_ESI)             79.0148 79.8118  93.52858 88.2432  97.6476  122.9255     5
# calcular_ingreso_medio_datatable <- ingreso_medio_datatable(base_final_ESI)       38.7052 42.2095  47.87350 43.6334  54.6335   60.1859     5
# calcular_ingreso_medio_datatable2 <- ingreso_medio_datatable2(base_final_ESI)     37.4621 41.0272  46.07150 41.3022  52.4413   58.1247     5


# ¿Existen diferencias importantes entre las distintas estrategias? 

# Sí, al utilizar data.table se puede ver que disminuye el tiempo de ejecución a menos de la mitad

# ¿Hay alguna más eficiente que otra? 

# Con data.table es mucho más eficiente y rápido 

# ¿Usar group_by versus map hace alguna diferencia?

# Por los resultados obtenidos no se ven grandes diferencias, los resultados son similares, la diferencia se da al utilizar data.table
