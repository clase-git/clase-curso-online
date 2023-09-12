

#Función extract_name
extract_name <- function(url) {
  file_name <- str_extract(url, "([^/]+)$")
  file_name <- str_replace(file_name, "\\?.*$", "")
  file_name <- str_replace_all(file_name, "[-]+personas", "")
  return(file_name)
}

#Función download_esi_data
download_esi_data <- function(url, file_name, directory) {
  file_path <- file.path(directory, file_name)
  download.file(url, destfile = file_path, mode = "wb")
}

#Función read_esi_data
read_esi_data = function(path){           
  fread(path, sep = "auto") 
}


weighted_percentile <- function(x, w, p) {
  sorted_data <- sort(x)
  sorted_weights <- w[order(x)]
  cum_weights <- cumsum(sorted_weights)
  target_position <- p * sum(w)
  index <- max(which(cum_weights <= target_position))
  percentile <- sorted_data[index] + 
    (target_position - cum_weights[index]) / sorted_weights[index] * (sorted_data[index + 1] - sorted_data[index])
  return(percentile)
}




#Funciones ejercicio 4

calculo_4.1 <- function(df) {
  df_filtrado <- filter(df, ocup_ref == 1)
  promedio_ponderado <- weighted.mean(df_filtrado$ing_t_p, w = df_filtrado$fact_cal_esi)
  return(data.frame(Version = paste0("esi_", unique(df$ano_trimestre)), Media = promedio_ponderado))
}


calculo_4.3 <- function(df) {
  dt <- as.data.table(df)
  dt_filtrado <- dt[ocup_ref == 1]
  promedio_ponderado <- weighted.mean(dt_filtrado$ing_t_p, w = dt_filtrado$fact_cal_esi)
  return(data.frame(Version = unique(paste0("esi_", df$ano_trimestre)), Media = promedio_ponderado))
}

