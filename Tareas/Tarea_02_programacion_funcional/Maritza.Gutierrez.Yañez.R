#Tarea clase 17-08-2023

library(gapminder)
library(purrr)
library(dplyr)
library(ggplot2)
library(rlang)
library(stringr)


#EJERCICIO 1


sum_something <- function (data, group_var,var){
 data %>%
    group_by({{ group_var }}) %>%
    summarise(n = sum({{ var }} ))
}


plot_table <- function(table, x_var, y_var, input_title) {
  ggplot(table, aes(x = {{ x_var }}, y = {{ y_var }})) +
    geom_bar(stat = "identity") +
    labs(title = input_title)
}


plot_with_purrr <- function(tablas) {
  plots_list <- tablas %>%
    map(function(plot) {
      table <- sum_something(plot, continent, pop)  
      plot_table(table, continent, n, paste("Población mundial, según continente. Año", plot$year[1]))
    })
  
  return(plots_list)
}


gapminder_list <- split(gapminder, gapminder$year)
plots <- plot_with_purrr(gapminder_list)
plots


#EJERCICIO 2

plot_table <- function(table, x_var, y_var, input_title,year) {
  ggplot(table, aes(x = {{ x_var }}, y = {{ y_var }})) +
    geom_bar(stat = "identity") +
    labs(title = input_title,
         subtitle = paste("Año",year))
}


plot_with_purrr <- function(tablas) {
  plots_list <- tablas %>%
    map(function(plot) {
      table <- sum_something(plot, continent, pop)  
      year <- plot$year[1]
      title <- paste("Población mundial, según continente.")
      plot_table(table, continent, n, title, year)
      })
  
  return(plots_list)
}


plots <- plot_with_purrr(gapminder_list)
plots


#EJERCICIO 3

#Función original
nested_for <- function(v1,v2){
  for (x in v1) {
    for (y in v2){
      print(paste(x,y))
    }
  }
}

nested_for (1:3, 5:8)


#Función requerida utilizando purrr
#Para vectores de igual o distinto largo 
#Para este ejercicio en términos de código es mas complejo utilizar purrr, en cuanto a resultado ambos cumplen su función. 

nested_map <- function(v1, v2) {
  max_len <- max(length(v1), length(v2))
  
  pmap(
    list(v1 = rep(v1, length.out = max_len), v2 = rep(v2, length.out = max_len)), #para que ambos vectores tengan la misma longitud
    ~ print(paste(.x, .y))
  )
}

nested_map(1:3, 5:8)
nested_map(1:3, 5:7)


#BONUS

files <- 
  list.files("C:/Users/Admin/Desktop/TAREA2/Bases", full.names = TRUE)

trimestres <- 
  list.files("C:/Users/Admin/Desktop/TAREA2/Bases") %>%
  stringr::str_extract(pattern = "-[[:digit:]]{2}-") %>%
  stringr::str_remove_all ("-")

bases_ene <- map(files, read.csv2)

names(bases_ene)<- paste0("trimestre_",trimestres)


nombres_lista <-
  imap(bases_ene, ~ .y)



#HACER UNA FUNCION QUE RECIBA UN DATAFRAME Y DEVUELVA LA CANTIDAD DE OCUPADOS
# activ=1 ocupados
# activ=2 desocupados
# activ=3 inactivos


#HACER EL GRAFICO

get_employment_sample <- function (data, group_var,var){
  data %>%
    group_by({{ group_var }}) %>%
    summarise(n = sum({{ var }} ))
}


plot_table <- function(table, x_var, y_var, input_title,filename) {
  p <-ggplot(table, aes(x = {{ x_var }}, y = {{ y_var }})) +
    geom_bar(stat = "identity") +
    labs(title = input_title)
  
  ggsave(filename = filename, plot = p)
}


plot_ocupados <- function(tablas) {
  plots_list <- tablas %>%
    map(function(plot) {
      table <- get_employment_sample(plot, activ, activ)  
      title <- paste("Población ocupada y no ocupada. Trimestre", plot$mes_central[1])
      filename <- paste("plot_",  plot$mes_central[1], ".jpg", sep = "")
      plot_table(table, activ, n, title, filename)
      
    })
  
  return(plots_list)
}


plots <- plot_ocupados(bases_ene)
plots





