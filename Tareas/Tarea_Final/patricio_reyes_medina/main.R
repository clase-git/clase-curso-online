# Paqueteria requerida

if(!require("purrr")) install.packages("purrr")
if(!require("purrr")) install.packages("purrr")
if(!require("dplyr")) install.packages("dplyr")
if(!require("microbenchmark")) install.packages("microbenchmark")
if(!require("data.table")) install.packages("data.table")

source("functions.R")

# Ejercicio 1 -------------------------------------------------------

urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)

file_names <- extract_name(urls)

purrr::pmap(list(urls, file_names, "data"), download_esi_data)

# Ejercicio 2 -------------------------------------------------------

ruta_data <- paste0("data/", file_names)

lista_esi <- purrr::map(ruta_data, read_esi_data)

# Ejercicio 3 -------------------------------------------------------

# Tabla 1
# Renombramos los elemnetos de la lista

stringr::str_extract(file_names, pattern = "\\d+")
names(lista_esi) <- paste0(
  "esi_", 
  stringr::str_extract(file_names, pattern = "\\d+"))

# Aplicar la función a cada elemento de la lista
tabla_1 <- lista_esi |>
  purrr::map(conteo_distintos) |> 
  dplyr::bind_rows(.id = "version")

print(tabla_1)

# Tabla 2
tabla_2 <- lista_esi |>
  purrr::map(estaditicos) |> 
  dplyr::bind_rows(.id = "version")

print(tabla_2)

# Respuestas: 
# claramente los pesos de muestreo aumentaron en la pandemia (año 2020), volviendo a valores más normales para el año 2021. Por otro lado, se muestran valores del factor de expansión que podrían ser atípicos como los máximos del 2019 y 2018 (18.284 y 19.321), habría investigar más sobre la normalidad de estos valor.

# Tabla 3
tabla_3 <- lista_esi |>
  purrr::map(fun_n_estrato) |> 
  dplyr::bind_rows(.id = "version")

print(tabla_3)

# Tabla 4
tabla_4 <- lista_esi |>
  purrr::map(estaditicos_ingreso) |> 
  dplyr::bind_rows(.id = "version")

print(tabla_4)

# Ejercicio 4  -------------------------------------------------------
# Se dejaran las bases solo con la variables de interes de estudio, para igualar la condición inicial.

# Preparación de bases

# Lista de tablas de esi 
lista_esi_resumida <- lista_esi |>
  purrr::map(function(x){x |> dplyr::select(ocup_ref, ing_t_p, fact_cal_esi)})

# Tabla apilada esi 
tabla_esi_apilada <- dplyr::bind_rows(
  lista_esi_resumida$esi_2021 |> dplyr::mutate(año = 2021),
  lista_esi_resumida$esi_2020 |> dplyr::mutate(año = 2020),
  lista_esi_resumida$esi_2019 |> dplyr::mutate(año = 2019),
  lista_esi_resumida$esi_2018 |> dplyr::mutate(año = 2018),
  lista_esi_resumida$esi_2017 |> dplyr::mutate(año = 2017),
  lista_esi_resumida$esi_2016 |> dplyr::mutate(año = 2016)
)
# Lista de tablas de esi en formato data.table
lista_esi_resumida_data.table <- lista_esi_resumida |>
  purrr::map(data.table::data.table)
 
# Tabla apilada esi en formato data.table
tabla_esi_apilada_data.table <- data.table::data.table(tabla_esi_apilada)

# Analisis:
 
# 1. Lista de tablas con purrr

resulta_1 <- microbenchmark::microbenchmark(
  lista_esi_resumida |>
    purrr::map(media_ingreso) |> 
    dplyr::bind_rows(.id = "version"),
  times = 5)
print(resulta_1)

# 2. Tablas apiladas con dplyr

resulta_2 <- microbenchmark::microbenchmark(
  tabla_esi_apilada |>
    dplyr::filter(ocup_ref == 1) |>
    dplyr::group_by(año) |>
    dplyr::summarise(media = weighted.mean(ing_t_p, fact_cal_esi)),
  times = 5)
print(resulta_2)

# 3. Lista de tabla en formato tabla.table con purrr

resulta_3 <- microbenchmark::microbenchmark(
  lista_esi_resumida_data.table |>
    purrr::map(media_ingreso_data.table) |> 
    dplyr::bind_rows(.id = "version"), # Esta función no tiene efecto aqui, pero dejo para igual condición.
  times = 5)
print(resulta_3)

# 4. Tablas apiladas en formato tabla.table con dplyr

resulta_4 <- microbenchmark::microbenchmark(
  tabla_esi_apilada_data.table[ocup_ref == 1, weighted.mean(ing_t_p, fact_cal_esi), by = "año"],
  times = 5)
print(resulta_4)

# Conclusiones

# Resultado 1 tiempo promedio: 18.8657 milisegundos
# Resultado 2 tiempo promedio: 19.53768 milisegundos
# Resultado 3 tiempo promedio: 11.89308 milisegundos
# Resultado 4 tiempo promedio: 10.93806 milisegundos
# Estos valores varian dependiendo el computador.

# Existe diferencia importantes, especialmente utilizando data.table que demostró ser más rápido.

# Data.table funciona de forma más eficiente con tablas apiladas en vez de lista de tablas.

# Por otro lado, con data frame es más eficiente usar map con lista de tablas que con tablas apiladas con group_by.




    
