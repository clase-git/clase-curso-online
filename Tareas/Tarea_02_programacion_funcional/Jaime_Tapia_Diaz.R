library(gapminder)
library(dplyr)
library(ggplot2)
library(rlang)
library(purrr)


#---------------EJERCICIO 1------------------

# crea función que recibe una tabla, variable para agrupar, variable a sumar y el año
sum_something <- function(data, group_var, var, year) {
  data %>% 
    group_by(!!enexpr(group_var), ano =!!enexpr(year)) %>% 
    summarise(n = sum(!!enexpr(var)))
}

# función recibe tabla, var eje x, var eje y, texto para titulo 
plot_table <- function(table, x_var, y_var, input_title ) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title = input_title)
}

# función recibe lista tabla para aplica a cada uno sum_something y plot_table por medio de map
plot_with_purrr <- function(tablas){  
  plots <- tablas %>% 
  map(~sum_something(.x, continent, pop, year)) %>% 
  map(~plot_table(.x, continent, n, paste("Población mundial, según continente -", .x$ano[1])))
  return(plots)
  }

# separa el data gapminder en listas según año
gapminder_list <- split(gapminder, gapminder$year)

# aplica función plot_with_purrr con gapminder_list
plot_with_purrr(gapminder_list)



#-----------------EJERCICIO 2------------------------

# crea función que recibe una tabla, variable para agrupar, variable a sumar y el año
sum_something <- function(data, group_var, var, year) {
  data %>% 
    group_by(!!enexpr(group_var), ano =!!enexpr(year)) %>% 
    summarise(n = sum(!!enexpr(var)))
}

# función recibe tabla, var eje x, var eje y, texto para titulo y el año para el subtitulo
plot_table <- function(table, x_var, y_var, input_title, ano ) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title = input_title, subtitle = ano)
}

# función recibe lista tabla para aplica a cada uno sum_something y plot_table por medio de map
plot_with_purrr <- function(tablas){  
  plots <- tablas %>% 
    map(~sum_something(.x, continent, pop, year)) %>% 
    map(~plot_table(.x, continent, n, "Población mundial, según continente.", .x$ano[1]))
  return(plots)
}

# separa el data gapminder en listas según año
gapminder_list <- split(gapminder, gapminder$year)

# aplica función plot_with_purrr con gapminder_list
plot_with_purrr(gapminder_list)



#-----------------EJERCICIO 3------------------------

# función recibe 2 vectores, lo transforma en lista y realiza las combinaciones entre sus elementos y luego imprime
nested_map <- function(v1, v2) {
  data <- list( v2, v1 )
  walk(data %>%
        cross() %>%
        map(lift(paste)), print)
}

nested_map(1:3, 5:8)



