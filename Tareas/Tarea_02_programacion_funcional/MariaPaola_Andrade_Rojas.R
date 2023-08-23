####### TAREA 2 ####### MARIA PAOLA ANDRADE ROJAS


#### EJERCICIO 1 ####

library(tidyverse)
library(gapminder)

sum_something <- function(gapminder, continent, pop) {
  gapminder |>
    group_by(!!enexpr(continent)) |>
    summarise(n = sum(!!enexpr(pop)))
}

tabla <- sum_something(gapminder, continent, pop)

plot_table <- function(tabla, x_pop, y_pop, input_title) {
  ggplot(tabla, aes(x = !!enexpr(x_pop), y = !!enexpr(y_pop))) +
    geom_bar(stat = "identity") +
    labs(title = input_title)
}

plot_table(tabla, continent, n, "Población mundial, según continente. Año")

######

library(dplyr)
library(purrr)
library(ggplot2)
library(rlang)

#1

gapminder_list <- split(gapminder, gapminder$year)

plot_with_for <- function(tablas){
  
  plots <- list(vector(length = length(tablas)))
  i <- 1
  for (plot in tablas) {
    table <- sum_something(plot, continent, pop)
    plots[[i]] <- plot_table(table, continent, n, paste("Población mundial, según continente. Año", plot$year[1]))
    i <- i + 1
  }
  return(plots)
}

plots <- plot_with_for(gapminder_list)
plots

####

gapminder_list <- split(gapminder, gapminder$year)

plot_with_purr <- function(tablas) {
  plots <- map(tablas, function(plot) {
    table <- sum_something(plot, continent, pop)
    plot_table(table, continent, n, paste("Población mundial, según continente. Año", plot$year[1]))
  })
  return(plots)
}

plots <- plot_with_purr(gapminder_list)
plots



#### EJERCICIO 2 ####


library(tidyverse)
library(gapminder)

sum_something <- function(gapminder, continent, pop) {
  gapminder %>%
    group_by(!!enexpr(continent)) %>%
    summarise(n = sum(!!enexpr(pop)))
}

# Modifica la función plot_table
plot_table <- function(tabla, x_pop, y_pop, subtitulo) {
  ggplot(tabla, aes(x = !!enexpr(x_pop), y = !!enexpr(y_pop))) +
    geom_bar(stat = "identity") +
    labs(subtitle = subtitulo)  # Modificamos "title" a "subtitle"
}


gapminder_list <- split(gapminder, gapminder$year)

plot_with_purrr <- function(tablas) {
  plots <- map(tablas, function(plot) {
    year <- plot$year[1]
    table <- sum_something(plot, continent, pop)
    subtitulo <- paste("Año", year)
    p <- plot_table(table, continent, n, subtitulo)  # Utilizamos "subtitulo" como subtítulo
    p + labs(title = "Población mundial, según continente")  # Agregar el título
  })
  return(plots)
}

plots_sub <- plot_with_purrr(gapminder_list)
plots_sub

#### EJERCICIO 3 ####

nested_for <- function(v1,v2) {
  for(x in v1) {
    for (y in v2) {
      print(paste(x,y))
    }
  }
}

nested_for(1:3, 5:8)

###### Modificación utilizando library(purrr)

#Sean v1 y v2

v1 <- 1:3
v2 <- 5:8

library(purrr)

nested_map <- function(v1, v2) {
  map(v1, function(x) {
    map(v2, function(y) {
      paste(x, y)
    })
  }) %>%
    flatten()
}

result_map <- nested_map(v1, v2)
result_map


#Para visualizar pares de valores generados

result_unlmap <- unlist(result_map)
result_unlmap

### Reflexiona sobre la pertinencia de purrr para tareas de este tipo.
# Respuesta: durante el desarrollo del ejercicio se determina mayor practicidad y facilidad 
#            para ingresar argumentos con bucles simples "for" para el caso presentado. 
#            Al utilizar funciones del paquete "purrr" resulta dificultad
#            al configurar iteración y restricción en longitud de vectores de distinto tamaño, 
#            fue necesario código más largo y menos intuitivo.

