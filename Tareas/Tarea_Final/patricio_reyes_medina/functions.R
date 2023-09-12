# Paqueteria -------------------------------------------------------

if(!require("readr")) install.packages("readr")
if(!require("stringr")) install.packages("stringr")
if(!require("dplyr")) install.packages("dplyr")

# Función extraer nombre -------------------------------------------

extract_name <- function(x) {
  stringr::str_extract(x, pattern = "esi-\\d{4}---personas.csv")
}

# Función descargar base de url -----------------------------------

download_esi_data <- function(url, file_name, directory) {
  options(timeout = 5000)
  download.file(
    url, 
    destfile = paste0(directory, "/", file_name))
}

# Función leer archivos csv ---------------------------------------

read_esi_data <- function(ruta_csv) {
  
  data <- tryCatch(
    readr::read_csv(ruta_csv),
    error = function(e) {
      read.csv(ruta_csv, stringsAsFactors = FALSE)
    }
  )
  
  return(data)
}

# Funciones para ejercicio 3

conteo_distintos <- function(data) {
  data |>
    dplyr::summarize(
      n_personas  = dplyr::n_distinct(idrph),
      n_hogares  = dplyr::n_distinct(id_identificacion)
    )
}

estaditicos <- function(data) {
  data |>
    dplyr::distinct(id_identificacion, .keep_all = TRUE) |> 
    dplyr::summarize(
      minimo  = min(fact_cal_esi),
      maximo  = max(fact_cal_esi),
      media  = mean(fact_cal_esi),
      mediana  = median(fact_cal_esi),
      p10  = quantile(fact_cal_esi, 0.1),
      p90  = quantile(fact_cal_esi, 0.9)
    )
}


fun_n_estrato <- function(data) {
  data |>
    group_by(estrato) %>%
    summarize(
      conglomerado_por_estrato = sum(n_distinct(conglomerado))
    ) %>%
    filter(conglomerado_por_estrato == 1) %>% 
    count()
}

estaditicos_ingreso <- function(data) {
  data |>
    dplyr::filter(ocup_ref == 1) |>
    dplyr::summarize(
      minimo  = min(ing_t_p),
      maximo  = max(ing_t_p),
      media  = weighted.mean(ing_t_p, fact_cal_esi),
      mediana  = quantile(ing_t_p, probs = .5, weights = fact_cal_esi),
      p10  = quantile(ing_t_p, probs = .1, weights = fact_cal_esi),
      p90  = quantile(ing_t_p, probs = .9, weights = fact_cal_esi)
    )
}

# Funciones para ejercicio 4

media_ingreso <- function(data) {
  data |>
    dplyr::filter(ocup_ref == 1) |>
    dplyr::summarize(
      media  = weighted.mean(ing_t_p, fact_cal_esi)
    )
}

media_ingreso_data.table <- function(data) {
  data[ocup_ref == 1, weighted.mean(ing_t_p, fact_cal_esi)] 
}














