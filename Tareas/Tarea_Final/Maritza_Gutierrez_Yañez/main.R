#SCRIPT PRINCIPAL TAREA FINAL

if (!require ("purrr")) {
  install.packages("purrr")
}

if (!require ("tidyverse")) {
  install.packages("tidyverse")
}

if (!require ("readr")) {
  install.packages("readr")
}

if (!require ("dplyr")) {
  install.packages("dplyr")
}

if (!require ("stringr")) {
  install.packages("stringr")
}

if (!require ("data.table")) {
  install.packages("data.table")
}

if (!require ("microbenchmark")) {
  install.packages("microbenchmark")
}


#EJECUTAR LAS FUNCIONES CREADAS PARA LOS EJERCICIOS
source("functions.R")


#EJERCICIO 1

urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)

          
          
#1.1
#Crear una función llamada extract_name que debe recibir una url y devuelva el nombre del archivo.


#crear un vector llamado file_names que almacene la porción de las url que contiene el nombre de los archivos 
file_names <- map(urls, extract_name)

print(file_names)

#1.2
#Crear una función llamada download_esi_data para descargar un archivo


# Ejemplo de uso de la función con los 3 parámetros especificados 
url <- "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true"
file_name <- "esi-2021---personas.csv"
directory <- "C:/Users/Admin/Desktop/R-Intermedio/Maritza_Gutierrez_Yañez"  # Ruta absoluta


download_esi_data(url, file_name, directory)



#1.3 
#Usando purrr, las url y el vector de nombres, descarga todos los archivos en una carpeta llamada data en tu directorio de trabajo.

#Función para descargar y guardar las bases en una carpeta


#Primero voy a crear la carpeta data si no existe

nombre_carpeta <- "data"

if (!file.exists(nombre_carpeta)) {
  dir.create(nombre_carpeta)
  cat("Carpeta 'data' creada en el directorio de trabajo.\n")
} else {
  cat("La carpeta 'data' ya existe en el directorio de trabajo.\n")
}

#Descargar y guardar

directorio <- "data"
walk2(urls, file_names, descargar_y_guardar)


#EJERCICIO 2  
#Crear una función llamada read_esi_data que lea un archivo. 


#Cargar los archivos en la sesión
#Lista de archivos de la carpeta data
lista_archivos <- list.files(path = "data", full.names = TRUE) 

#Aplicar la función read_esi_data a cada archivo de la lista
bases_esi <- map(lista_archivos, read_esi_data)



#EJERCICIO 3

#3.1
#Tabla que contenga 3 columnas: version, n_personas (idrph) y n_hogares (id_identificacion). 

base_combinada <- rbindlist(bases_esi, use.names = TRUE, fill = TRUE) #Combinar las bases


tabla1 <- base_combinada[, .(n_personas = uniqueN(idrph), n_hogares = uniqueN(id_identificacion)),
                              by = .(anio = ano_trimestre)][, .(version = paste0("esi_", anio), n_personas, n_hogares)]
print(tabla1)

#3.2
#Tabla que contenga mínimo, máximo, media, mediana, p10 y p90 del factor de expansión (fact_cal_esi) para cada versión. 

  
#Agregarle a la base la columna version
base_combinada [, version := paste0("esi_", ano_trimestre)] 

#Considerar solo una fila por hogar (id_identificacion)
base_combinada_sin_duplicados <- unique(base_combinada, by = "id_identificacion")

tabla2 <- base_combinada_sin_duplicados[, .(min_fact_cal_esi = min(fact_cal_esi),
                             max_fact_cal_esi = max(fact_cal_esi),
                             media_fact_cal_esi = mean(fact_cal_esi),
                             mediana_fact_cal_esi = median(fact_cal_esi),
                             p10_fact_cal_esi = quantile(fact_cal_esi, 0.10),
                             p90_fact_cal_esi = quantile(fact_cal_esi, 0.90)),
                             by = version]

print(tabla2)

#¿Se observan algunos pesos de muestreo atípicos?
# Los valores del factor de expansión son totalmente distintos, el rango es amplio ya que el mínimo y el máximo están bastante distanciados.
# En los años 2020 y 2021 se observa una disminución considerable en los máximos, lo que se evidenció también en la media y la mediana.


#3.3
#Tabla que contenga el número de estratos (estrato) con una sola unidad primaria de muestro (conglomerado). 

tabla3 <- base_combinada[conglomerado == 1, .(num_estratos = .N), by = .(version, estrato)]

print(tabla3)

#No hay estratos con una sola unidad primaria de muestreo



#3.4
#Tabla que contenga mínimo, máximo, media, mediana, p10 y p90 de los ingresos del trabajo principal (ing_t_p) para cada versión.

tabla4 <- base_combinada[ocup_ref == 1 , .(min_ingresos = min(ing_t_p*fact_cal_esi),
                                           max_ingresos = max(ing_t_p*fact_cal_esi),
                                           media_ingresos = mean(ing_t_p*fact_cal_esi),
                                           mediana_ingresos = median(ing_t_p*fact_cal_esi),
                                           p10_ingresos = quantile(ing_t_p*fact_cal_esi, 0.10),
                                           p90_ingresos = quantile(ing_t_p*fact_cal_esi, 0.90)),
                                           by = version]


print(tabla4)



#EJERCICIO 4
#Calcula el promedio de ingresos en las tablas de la ESI (ing_t_p) mediante las siguientes estrategias:


#4.1- Estrategia 1
#Lista de tablas: calcular promedio con herramientas de purrr 

#Función para calcular el promedio

#Calcular el promedio
promedio_ej_4.1 <- map(bases_esi, calcular_promedio)



#4.2- Estrategia 2 
#Tablas apiladas: calcular promedio con group_by() %>% summarise() (apila una tabla sobre otra en un dataframe)

promedio_ej_4.2 <- base_combinada %>%
  filter(ocup_ref == 1) %>%
  group_by(version) %>%
  summarise(promedio_ingresos = mean(ing_t_p*fact_cal_esi))


#4.3- Estrategia 3
#Lista de tablas: calcular promedio con herramientas de purrr, utilizando una función creada por ti, que utilice data.table.

#Función para calcular el promedio


#Calculo del promedio
promedio_ej_4.3 <- map_dbl(bases_esi, calcular_promedio_dt)

print(promedio_ej_4.3)

#4.4- Estrategia 4
#Tablas apiladas: calcular promedio con data.table.

promedio_ej_4.4 <- base_combinada[ocup_ref == 1, .(promedio_ingresos = mean(ing_t_p*fact_cal_esi)), by = version]




#COMPARACION

comparacion <- microbenchmark(
  # Estrategia 1
  promedio_ej_4.1 = {
    promedio_ej_4.1 <- map(bases_esi, calcular_promedio)
  },
  
  # Estrategia 2
  promedio_ej_4.2 = {
    promedio_ej_4.2 <- base_combinada %>%
      filter(ocup_ref == 1) %>%
      group_by(version) %>%
      summarise(promedio_ingresos = mean(ing_t_p*fact_cal_esi))
  },
  
  # Estrategia 3
  promedio_ej_4.3 = {
    promedio_ej_4.3 <- map(bases_esi, calcular_promedio_dt)
  },
  
  # Estrategia 4
  promedio_ej_4.4 = {
    promedio_ej_4.4 <- base_combinada[ocup_ref == 1, .(promedio_ingresos = mean(ing_t_p*fact_cal_esi)), by = version]
  },
  
  times = 5  # Número de iteraciones
)

# Resultado de la comparacion
print(comparacion)

##¿Existen diferencias importantes entre las distintas estrategias? 
#Sí, hay diferencias importantes entre las estrategias, se observa diversidad en los tiempos de ejecución de cada estrategia.

##¿Hay alguna más eficiente que otra? 
#Sí, la estrategia 4 (data.table) es la más eficiente ya que tiene el menor tiempo de ejecución y su máximo no esta distante al mínimo; además se encuentra aún debajo de los tiempos de las otras estrategias, contando así con el mejor rendimiento.
#Las estrategias 2, 1 y 3 los tiempos de ejecución son más largos no siendo eficientes al comparar con la otra estrategia. 

##¿Usar group_by (estrategia 2) versus map (estrategias 1 y 3) hace alguna diferencia?
#Sí, en este ejercicio al usar group_by los tiempos de ejecución resultaron ser más cortos, por lo tanto, más eficientes que al usar map.
#Como dato, esto depende del código porque anteriormente había realizado el ejercicio con otros códigos y los resultados fueron distintos y resultaba más conveniente usar map que demora menos en realizar los cálculos y además las líneas de código son más simples.
