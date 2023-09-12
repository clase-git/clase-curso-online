if(!require("stringr")){
  install.packages("stringr")
  library(dplyr)
} 

if(!require("xfun")){
  install.packages("xfun")
  library(xfun)
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
  library(readr)
} 


source("functions.R")


# EJERCICIO 1 -------------------------------------------------------------


urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/1esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true")

##modificamos las url
## Elimina el "1" antes de "esi-2017"
urls <- gsub("1esi", "esi", urls)

## vector con nombres de archivos que descargaremos
file_names <- map_chr(urls, extract_name)

#CREAMOS CARPETA DATA
carpeta<-dir.create("data")
#DESCARGAMOS ARCHIVOS EN CARPETA

descarga<-map(urls,funcion_descarga)



# EJERCICIO 2 -------------------------------------------------------------

#####Leemos todos los archivos de la carpeta data########
files_esi<-list.files("data/",full.names = T)
esi<-map(files_esi,~read_esi_data(.x))
nombres<-str_extract_all(files_esi,"(2016|2017|2018|2019|2020|2021)") %>% str_replace_all("---","-")
names(esi)<-paste0("esi_",nombres)

#Creamos un vector para luego añadirlo como variable a cada una de las bases de datos
version<-paste0("esi_",nombres)



# EJERCICIO 3 -------------------------------------------------------------

#EJERCICIO 3.1###generamos una nueva variable llamada version
esi<-map2(esi,version,~mutate(.x,version=.y))
table_1 <- esi %>%
  map(~ summarise(., version = paste0("esi_", unique(.$ano_trimestre)), 
                  n_personas = n_distinct(.data$idrph), 
                  n_hogares = n_distinct(.data$id_identificacion))) %>%
  bind_rows()
#obtenemos tabla 1
table_1

##EJERCICIO 3.2 
table_2<- esi %>% map(resumen_variable) %>%  
  bind_rows(.id = "version")

print(table_2)

##EJERCICIO 3.3

estratos_conglomerado <- esi %>%
  map(~ group_by(., version, conglomerado) %>%
        summarise(n_estratos = table(estrato)) %>% filter(n_estratos==1) %>% 
        summarise(n_estratos1=table(n_estratos)))
tabla_3.3<-bind_rows(estratos_conglomerado)
tabla_3.3

##EJERCICIO 3.4 utilizando ocup_ref==1 que son los ocupados de referencia

table_3.4<-esi %>% 
  map(resumen_ingresos) %>%
  bind_rows(.id = "version")




# EJERCICIO 4 -------------------------------------------------------------


#Funciones ejercicio 4

#4.1- Estrategia 1
#Lista de tablas: calcular promedio con herramientas de purrr 

#Función para calcular el promedio

#Calcular el promedio
prueba1 <- map(esi,promedio) %>% bind_rows(.id="version")



#4.2- Estrategia 2 
#Tablas apiladas: calcular promedio con group_by() %>% summarise() (apila una tabla sobre otra en un dataframe)
#combinamos las bases de la esi

esi_unica<- rbindlist(esi, use.names = TRUE, fill = TRUE) #Combinar las bases

prueba2 <- esi_unica %>%
  filter(ocup_ref == 1) %>%
  group_by(version) %>%
  summarise(promedio = weighted.mean(ing_t_p, fact_cal_esi))


#4.3- Estrategia 3
#Lista de tablas: calcular promedio con herramientas de purrr, utilizando una función creada por ti, que utilice data.table.

#Función para calcular el promedio


#Calculo del promedio
prueba3<- map(esi, promedio_3)%>%
  bind_rows(.id="version")

#4.4- Estrategia 4
#Tablas apiladas: calcular promedio con data.table.
esi_unica<- rbindlist(esi, use.names = TRUE, fill = TRUE) #Combinar las bases
prueba4 <- esi_unica[ocup_ref == 1, .(promedio = weighted.mean(ing_t_p, fact_cal_esi)), by = version]

#Respuestas a preguntas

comparar_pruebas <- microbenchmark(
  PRUEBA1 = prueba1,
  PRUEBA2 = prueba2,
  PRUEBA3 = prueba3,
  PRUEBA4 = prueba4,
  times = 5)


print(comparar_pruebas)

#¿Existen diferencias importantes entre las distintas estrategias? 
# NO SE VE MUCHA DIFERENCIA ENTRE LAS DISTINTAS ESTRATEGIAS, PERO LA PRUEBA 2 Y 3 SON UN POCO MAS EFICIENTES QUE LAS DEMAS
# SE MEZCLA DATA.TABLE CON MAP Y GROUP_BY.

#¿Hay alguna más eficiente que otra? 
# LA MÁS EFICIENTE ES LA QUE UTILICÉ PURRR CON DATA.TABLE  QUE ES LA PRUEBA 3. 

#¿Usar group_by versus map hace alguna diferencia?
# map fue más eficiente que group_by pero por poca diferencia.