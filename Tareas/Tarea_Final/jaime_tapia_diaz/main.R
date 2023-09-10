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

#if(!require("qdapRegex")) install.packages("qdapRegex")
#library(qdapRegex)

if(!require("microbenchmark")){
  install.packages("microbenchmark")
  library(microbenchmark)
} 



source(paste0(getwd(),"/","functions.R"))         # incorpora el script "functions.R"


###################### EJERCICIO 1 ###########
# ███ ███ ███ ███ ███ ███ ███ ███ ███   ██ 
# █     █ █   █ █ █    █  █    █  █ █    █ 
# ██    █ ██  ██  █    █  █    █  █ █    █ 
# █   █ █ █   █ █ █    █  █    █  █ █    █ 
# ███ ███ ███ █ █ ███ ███ ███ ███ ███   ███
##############################################

urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)

# ------ F(x)  extract_name --------------------------------------------------------------------------------------------------------------------

file_names <- map(urls, extract_name)             # se crea lista de nombres de archivo.


# ------ F(x) download_esi_data --------------------------------------------------------------------------------------------------------------------

download_esi_data(urls,file_names[])              # baja los archivos de los urls.



################ EJERCICIO 2 ################
# ███ ███ ███ ███ ███ ███ ███ ███ ███   ██ 
# █     █ █   █ █ █    █  █    █  █ █     █ 
# ██    █ ██  ██  █    █  █    █  █ █    █ 
# █   █ █ █   █ █ █    █  █    █  █ █   █ 
# ███ ███ ███ █ █ ███ ███ ███ ███ ███   ███
#############################################

# ------ F(x) leer archivos --------------------------------------------------------------------------------------------------------------------
# Crea una función llamada read_esi_data que lea un archivo. La función recibe como argumento la ruta del archivo (ej: data/esi-2018—personas.csv).
# read_esi_data debe ser capaz de reconocer el tipo de separador y leer el archivo correctamente en todos los casos.

directory <- "data"

directory <- paste0(getwd(),"/",directory)                                              # compagina el directorio de trabajo con el directorio del argumento.

list_esi_files<-as.list(list.files(path = directory,pattern = "esi-[a-z,0-9,-]+.csv"))  # obtiene lista de archivos csv en el directorio.

esi_data <- map( paste0(directory,"/",list_esi_files), read_esi_data)                    # se utiliza la función para lee cada uno de los archivos de la lista de archivos.
                                                                                         # salva lista en esi_data.


################ EJERCICIO 3 ################
# ███ ███ ███ ███ ███ ███ ███ ███ ███   ██ 
# █     █ █   █ █ █    █  █    █  █ █     █ 
# ██    █ ██  ██  █    █  █    █  █ █   ██ 
# █   █ █ █   █ █ █    █  █    █  █ █     █ 
# ███ ███ ███ █ █ ███ ███ ███ ███ ███   ██
#############################################

# ------ F(x)  Tabla 3.1 --------------------------------------------------------------------------------------------------------------------
# Tabla que contenga 3 columnas:version,n_personas(idrph) y n_hogares(id_identificacion). En la columna version debes usar la siguiente estructura: esi_{año}. Ejemplo: esi_2017

tabla_3.1 <- esi_data[] %>%
              map(.f= ~summarise (.x, version = max(ano_encuesta),                # selecciona el max de la columna año (siempre va ser el mismo).
                                  n_personas = n_distinct(idrph),                 # cuenta los registros idrph sin repetir.
                                  n_hogares = n_distinct(id_identificacion))) %>% # cuenta los registros id_identificacion sin repetir.
              map(.f = ~mutate(.x, version = paste0("esi_",version)))             # antepone "esi_" a los años, cambia nombre a versión.

tabla_3.1 <- bind_rows(tabla_3.1[])                                               # apila todas las tablas para presentar.
tabla_3.1[]


# ------ F(x)  Tabla 3.2 --------------------------------------------------------------------------------------------------------------------
# Tabla que contenga mínimo, máximo, media, mediana, p10 y p90 del factor de expansión (fact_cal_esi) para cada versión.
# Debes considerar una fila por hogar (id_identificacion) e incluir la columna version.
# ¿Se observan algunos pesos de muestreo atípicos?
# R. A pesar que los dos ultimos años (2020 y 2021) tengan un maximo mucho menor a los años anteriores, el que destaca es el 2020.
#    Ya que su media, mediana, p10 y p90 sobresalen por mucho al resto de los años.

tabla_3.2 <- esi_data[] %>% 
              map(.f = ~select(.x, ano_encuesta, id_identificacion, fact_cal_esi)) %>%    # selecciona columnas necesarias para aliviar el trabajo.
              map(.f = ~mutate(.x, id_identificacion = as.double(id_identificacion))) %>% # fuerza conversión a numérico double para evitar errores.
              map(.f = ~distinct(.x)) %>%                                                 # elimina las filas que tengan los mismos datos, así tenemos registros solo por hogar.
              map(.f = ~summarise(.x, version = max(ano_encuesta),
                                  minimo = min(fact_cal_esi),
                                  maximo = max(fact_cal_esi),
                                  media = mean(fact_cal_esi),
                                  mediana  = median(fact_cal_esi),
                                  p10 = quantile(fact_cal_esi, .1),
                                  p90 = quantile(fact_cal_esi, .9))) %>%                  # se calculan los resultados solicitados en función a fact_cal_esi.
              map(.f = ~mutate(.x, version = paste0("esi_",version)))                     # antepone "esi_" a los años de la columna versión.

tabla_3.2 <- bind_rows(tabla_3.2[])                                                       # apila todas las tablas para presentar.
tabla_3.2[]


# ------ F(x)  Tabla 3.3 --------------------------------------------------------------------------------------------------------------------
# Tabla que contenga el número de estratos (estrato) con una sola unidad primaria de muestro(conglomerado). Debes incluir la columna version.

tabla_3.3 <- esi_data[] %>%
              map(.f = ~select(.x, ano_encuesta, estrato, conglomerado)) %>%              # selecciona columnas necesarias para aliviar el trabajo.
              map(.f = ~mutate(.x, ano_encuesta = paste0("esi_",ano_encuesta))) %>%       # antepone "esi_" a los años de la columna ano_encuesta.
              map(.f = ~distinct(.x)) %>%                                                 # elimina las filas que tengan los mismos datos, asi tenemos repeticiòn de estratos y conglomeracion (solo una vez).
              map(.f = ~count(.x, ano_encuesta, estrato, name = "n_conglomerados")) %>%   # cuenta las conglomeraciones que tienen cada estrato.
              map(.f = ~filter(.x, n_conglomerados == 1)) %>%                             # filtra donde el numero de conglomeraciones sea 1.
              map(.f = ~summarise(.x, version = max(ano_encuesta),
                                  n_estratos_1cong = sum(n_conglomerados)))               # suma la cantidad de estratos que quedaron después del filtro.

tabla_3.3 <- bind_rows(tabla_3.3[])                                                       # apila todas las tablas para presentar.
tabla_3.3[]


# ------ F(x)  Tabla 3.4 --------------------------------------------------------------------------------------------------------------------
# Tabla que contenga mínimo, máximo, media, mediana, p10 y p90 de los ingresos del trabajo principal(ing_t_p) para cada versión.
# Esta tabla debe ser construida a nivel persona, utilizando el factor de expansión (fact_cal_esi). 

tabla_3.4 <- esi_data[] %>%
              map(.f= ~select(.x, ano_encuesta, ing_t_p, fact_cal_esi, ocup_ref)) %>%     # selecciona columnas necesarias para aliviar el trabajo.
              map(.f = ~filter(.x, ocup_ref == 1)) %>%                                    # filtra donde el ocup_ref sea 1.
              map(.f = ~mutate(.x, ing_expand := ing_t_p * fact_cal_esi,
                               ano_encuesta = paste0("esi_",ano_encuesta))) %>%           # se agrega columna con calculo del ingreso expandido y se antepone "esi_" al año.
              map(.f = ~summarise(.x, version = max(ano_encuesta),
                                  minimo = min(ing_expand),
                                  maximo = max(ing_expand),
                                  media = mean(ing_expand),
                                  mediana  = median(ing_expand),
                                  p10 = quantile(ing_expand, .1),
                                  p90 = quantile(ing_expand, .9)))                        # se calculan los resultados solicitados en funcion a ing_expand.

tabla_3.4 <- bind_rows(tabla_3.4[])                                                       # apila todas las tablas para presentar.
tabla_3.4[]


################ EJERCICIO 4 ################
# ███ ███ ███ ███ ███ ███ ███ ███ ███   █ █ 
# █     █ █   █ █ █    █  █    █  █ █   █ █ 
# ██    █ ██  ██  █    █  █    █  █ █   ███ 
# █   █ █ █   █ █ █    █  █    █  █ █     █ 
# ███ ███ ███ █ █ ███ ███ ███ ███ ███     █
#############################################

# ------ F(x)  Metodo 1 --------------------------------------------------------------------------------------------------------------------
# Lista de tablas: calcular promedio con herramientas de purrr (como en el ejercicio anterior).
metodo_1(esi_data[])[]

# ------ F(x)  Metodo 2 --------------------------------------------------------------------------------------------------------------------
# Tablas apiladas: calcular promedio con group_by() %>% summarise() (apila una tabla sobre otra en un dataframe).
metodo_2(esi_data[])[]

# ------ F(x)  Metodo 3 --------------------------------------------------------------------------------------------------------------------
# Lista de tablas: calcular promedio con herramientas de purrr, utilizando una función creada por ti, que utilice data.table.
metodo_3(esi_data[])[]

# ------ F(x)  Metodo 4 --------------------------------------------------------------------------------------------------------------------
# Tablas apiladas: calcular promedio con data.table.
metodo_4(esi_data[])[]


# ------ F(x)  Benchmarking the Performance of R Code ----------------------------------------------------------------------------------------

# haciendo benchmaking.
results <- microbenchmark(
  metodo_1_test = metodo_1(esi_data[]),
  metodo_2_test = metodo_2(esi_data[]),
  metodo_3_test = metodo_3(esi_data[]),
  metodo_4_test = metodo_4(esi_data[]),
  times = 5)

results   # muestra resultados

# ¿Existen diferencias importantes entre las distintas estrategias?
# R. Tomando en cuenta el promedio de tiempo (mean), se puede observar que los métodos (1 y 3) que operan con "purrr" son más
#    rápidos que los demás (2 y 4). Por lo general, la diferencia de tiempo ronda el 60%.

# ¿Hay alguna más eficiente que otra?
# R. Por lo general, el método 1 (puramente "purrr") fue más veloz que los demás, pero con poca diferencia respecto al método 3 ("purrr" y "data.table").  

# ¿Usar group_by versus map hace alguna diferencia?
# R. De acuerdo a lo observado, existe una gran disparidad entre "group_by" (método 2) y "map" (métodos 1 y 3).
#    La diferencia entre ellos ronda, en promedio, un 60%, donde los métodos "map" resultaron más rápidos.


