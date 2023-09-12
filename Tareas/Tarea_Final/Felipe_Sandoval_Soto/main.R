### Principal

source("functions.R") ## lee librerias y carga las funciones

## Aquí cabde destacar que en el caso de la esi_2017, la url tenía un error
## "iesi-2017" se le borró la "i". 
## Además, para efectos prácticos se borró todo lo que viene después de ".csv" ya 
## que no era necesario, los archivos se descargan igual.
urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv"
)

### EJERCICIO 1 
file_names <- extract_name(urls)
download_esi_data(urls,file_names,"data") ## puede usar el nombre que quiera para la carpeta donde se guardarán los archivos, si no existe la creará

### EJERCICIO 2
## Se pueden leer los datos uno por uno y quedan en tablas separadas
# esi_2016 <- read_esi_data("data/esi-2016---personas.csv")
# esi_2017 <- read_esi_data("data/esi-2017---personas.csv")
# esi_2018 <- read_esi_data("data/esi-2018---personas.csv")
# esi_2019 <- read_esi_data("data/esi-2019---personas.csv")
# esi_2020 <- read_esi_data("data/esi-2020---personas.csv")
# esi_2021 <- read_esi_data("data/esi-2021---personas.csv")

## Se guardan las rutas de los archivos y se crea el archivo esi en formato lista
files <- list.files("data/", full.names = T)
anios <- list.files("data/") %>% 
  stringr::str_extract(pattern = "[0-9]{4}")
esi <-  purrr::map(files, read_esi_data)
names(esi) <- paste0("esi_", anios)

## Como tablas apiladas, debido a que las BBDD no tienen las mismas variables, debemos hacer una seleción
variables <- c("version","conglomerado","estrato","ocup_ref","fact_cal_esi","ing_t_p")
esi1 <- purrr::map(esi, ~{dplyr::select(.x,all_of(variables))})
esi2 <- data.table(dplyr::bind_rows(esi1))

### EJERCICIO 3

tabla3.1.1 <- purrr::map(esi, tabla_pers_hog) ## sobre tablas en lista
## se puede hacer también tabla por tabla, pero no sobre los datos apilados,
## ya que id_identificacion tiene distintos formatos entre distintas esi

tabla3.2.1 <- purrr::map(esi,tabla_fact_cal) ## sobre tablas en lista
tabla3.2.2 <- tabla_fact_cal(esi2)           ## sobre tablas apiladas
## aquí se puede ver dentro de la tabla que los máximos son excesivamente altos,
## sin embargo, esto son sólo casos aislados ya que al observar la media y la 
## mediana se puede ver que se mantienen controlados. 
## Además, se observa dado los valores de p10 y p90 que en los años 2017, 2018 y 
## 2019 los datos son atípicos al mostrar valores más altos en p10 que en p90.

tabla3.3.1 <- purrr::map(esi, tabla_est_con) ## sobre tablas en lista
tabla3.3.2 <- tabla_est_con(esi2)            ## sobre tablas apiladas

tabla3.4.1 <- purrr::map(esi, tabla_ingreso)## considerando todas las personas 
tabla3.4.2 <- purrr::map(esi, ~{tabla_ingreso(subset(.x,ocup_ref==1))}) ## considerando solo las personas ocupadas
## en este caso no se puede aplicar sobre tablas aplicadas al usar survey

######EJERCICIO 4 
## como aclaración, se utilizó el filtro ocup_ref==1 para considerar sólo los ocupados
## para poder compararlos con los datos entregados en la publicación de la esi

## Ejecutamos ejer4.1, para comparar los tiempos
ejer4.1 <- microbenchmark(
    lista1  = purrr::map(esi,~{tabla_ingreso(subset(.x,ocup_ref==1))}),
    tabla2  = esi2 %>% 
              dplyr::mutate(ingreso=fact_cal_esi*ing_t_p) %>%
              dplyr::filter(ocup_ref==1) %>% 
              dplyr::group_by(version) %>% 
              dplyr::summarise(sum(ingreso)/sum(fact_cal_esi)),
    lista3  = purrr::map(esi,~{ingreso_medio(subset(.x,ocup_ref==1))}),
    tabla4  = esi2[,ingreso:=fact_cal_esi*ing_t_p,
                  ][ocup_ref==1,,][,sum(ingreso)/sum(fact_cal_esi),by=version],
    times = 5)


## una variación del ejercicio considerando esi1 (con solo las variables necesarias)
ejer4.2 <- microbenchmark(
  lista1  = purrr::map(esi1,~{tabla_ingreso(subset(.x,ocup_ref==1))}),
  tabla2  = esi2 %>% 
    dplyr::mutate(ingreso=fact_cal_esi*ing_t_p) %>%
    dplyr::filter(ocup_ref==1) %>% 
    dplyr::group_by(version) %>% 
    dplyr::summarise(sum(ingreso)/sum(fact_cal_esi)),
  lista3  = purrr::map(esi1,~{ingreso_medio(subset(.x,ocup_ref==1))}),
  tabla4  = esi2[,ingreso:=fact_cal_esi*ing_t_p,
                ][ocup_ref==1,,][,sum(ingreso)/sum(fact_cal_esi),by=version],
  times = 5)

# Al analizar los resultados de ejer4.1 y ejer4.2 se puede ver claramente que en los
# casos que utilizamos map los códigos son menos eficientes. Ahora bien, en el caso
# de lista1 claramente la eficiencia se ve afectada al utilizar el paquete survey
# para muestras complejas, sin embargo, se obtiene más información con la función tabla_ingreso.
# De todas maneras la más eficiente es tabla4 que se utiliza sobre tablas apiladas 
# usando funciones de la librería data.table. 
# Por otra parte, podemos notar que al utilizar las tablas con los datos seleccionados
# lista3 es mucho más eficiente (cerca del 85% menos de tiempo) aunque aún sigue siendo
# cerca del doble menos eficiente que al usar tabla4 

