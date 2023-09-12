###################### EJERCICIO 1 ###########
# ------ F(x)  extract_name --------------------------------------------------------------------------------------------------------------------

extract_name = function(string){
  string <- str_extract_all(string,
                            pattern = "esi-[a-z,0-9,-]+.csv")
  unlist(string)                                                # convierte string a vector para usar el map de purrr, tal como solicita esta tarea (la verdad que no es necesario)
}

# ------ F(x) download_esi_data --------------------------------------------------------------------------------------------------------------------

download_esi_data = function(url, file_name, directory = "data"){
  old_wd <- getwd()                                               # rescata path de trabajo del usuario
  if(!dir.exists(directory)) {dir.create(directory)}              # crea directorio
  setwd(paste0(getwd(),"/",directory))                            # setea path de trabajo al directorio
  options(timeout = max(300, getOption("timeout")))               # para que alcance a bajar los archivos pesados
  map2(url,file_name[], download.file)                            # utiliza purrr para descarga todos los archivos a la carpeta
  setwd(old_wd)                                                   # vuelve a directorio de trabajo original
}


################ EJERCICIO 2 ################
# ------ F(x) leer archivos --------------------------------------------------------------------------------------------------------------------

read_esi_data = function(path){           # función solicitada
                fread(path, sep = "auto") # lee archivo csv según ruta y autoidentificación de separador
}

################ EJERCICIO 3 ################





################ EJERCICIO 4 ################
# ------ F(x)  Metodo 1 --------------------------------------------------------------------------------------------------------------------

metodo_1 = function(esi_data) {
  temp_t <- esi_data[] %>%
    map(.f= ~select(.x, ano_encuesta, ing_t_p, fact_cal_esi, ocup_ref)) %>%       # selecciona columnas necesarias para aliviar el trabajo
    map(.f = ~filter(.x, ocup_ref == 1)) %>%                                      # filtra donde el ocup_ref sea 1
    map(.f = ~mutate(.x, ing_expand := ing_t_p * fact_cal_esi)) %>%               # se calcula ing_expand con "ing_t_p" multiplicado por "fact_cal_esi"
    map(.f = ~summarise(.x, ano_encuesta = max(ano_encuesta),                     # calcula promedio de ing_expand
                        promedio = mean(ing_expand)))
  temp_t <- bind_rows(temp_t[])                                             # apila todas las tablas para presentar
}



# ------ F(x)  Metodo 2 --------------------------------------------------------------------------------------------------------------------

metodo_2 = function(esi_data) {
  temp_t <- esi_data[]
  for (i in 1:length(temp_t[])){                                                # bucle para seleccionar las mismas columnas en todas las tablas
    temp_t[[i]] <- temp_t[[i]] %>%
      select(ano_encuesta, ing_t_p, fact_cal_esi, ocup_ref)       # selecciona columnas necesarias para aliviar el trabajo
  }
  temp_t <- bind_rows(temp_t[])                                                 # se apilan las tablas de los distintos años de la esi
  temp_t <- temp_t %>%
    filter(ocup_ref == 1) %>%                                         # filtra donde el ocup_ref sea 1
    mutate(ing_expand = ing_t_p * fact_cal_esi) %>%                   # se calcula ing_expand con "ing_t_p" multiplicado por "fact_cal_esi"
    group_by(ano_encuesta) %>%                                        # se agrupa por año
    summarise(promedio = mean(ing_expand))                            # calcula promedio de ing_expand
}


# ------ F(x)  Metodo 3 --------------------------------------------------------------------------------------------------------------------

func_creada_por_mi = function(table){                                           # función que utiliza data.table
  table <- data.table(table)[, ing_expand := ing_t_p * fact_cal_esi             # se calcula ing_expand con "ing_t_p" multiplicado por "fact_cal_esi"
  ][,.(ano_encuesta = max(ano_encuesta),              
       promedio = mean(ing_expand))]                      # calcula promedio de ing_expand
}

metodo_3 = function(esi_data) {
  temp_t <- esi_data[] %>%
    map(.f= ~select(.x, ano_encuesta, ing_t_p, fact_cal_esi, ocup_ref)) %>%       # selecciona columnas necesarias para aliviar el trabajo
    map(.f = ~filter(.x, ocup_ref == 1)) %>%                                      # filtra donde el ocup_ref sea 1
    map(.f = ~func_creada_por_mi(.x))                                             # llama a la función para calcular promedio de ingreso para cada tabla
  
  temp_t <- bind_rows(temp_t[])                                                 # apila todas las tablas para presentar
}


# ------ F(x)  Metodo 4 --------------------------------------------------------------------------------------------------------------------

metodo_4 = function(esi_data) {
  temp_t <- esi_data[]
  for (i in 1:length(temp_t[])){                                               # bucle para seleccionar las mismas columnas en todas las tablas
    temp_t[[i]] <- temp_t[[i]] %>%
      select(ano_encuesta, ing_t_p, fact_cal_esi, ocup_ref)                       # selecciona columnas necesarias para aliviar el trabajo
  }
  temp_t <- bind_rows(temp_t[])                                             # se apilan las tablas de los distintos años de la esi
  temp_t <- temp_t[ocup_ref == 1,                                           # filtra donde el ocup_ref sea 1
  ][, ing_expand := ing_t_p * fact_cal_esi                # se calcula ing_expand con "ing_t_p" multiplicado por "fact_cal_esi"
  ][,.(promedio = mean(ing_expand)),                      # calcula promedio de ing_expand
    by = ano_encuesta]                                  # se agrupa por año
}


