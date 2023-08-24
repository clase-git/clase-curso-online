##### Tarea 2 ####


library(gapminder)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(purrr)
library(feather)


###previo funciones de la clase

sum_something <- function(data, group_var, var) {
  data %>% 
    group_by(!!enexpr(group_var)) %>% 
    summarise(n = sum(!!enexpr(var)))
}


plot_table <- function(data, x_var, y_var,  input_title )  {
  
  ggplot(data, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title=input_title)
}

#función para graficos que contenga el año
gapminder_list <- split(gapminder,gapminder$year)


##### Ejercicio 1  

plot_with_purrr <- function(tablas) {
  imap(tablas, ~ {
    table <- sum_something(.x, continent, pop)
    input_title <- paste("Población mundial, según continente. Año", .y)
    plot_table(table, continent, n, input_title)
  })
}

plots <- plot_with_purrr(gapminder_list)

print(plots)

##### Ejercicio 2

#modifico plot_table para agregar subtitulo

plot_table <- function(data, x_var, y_var, title, subtitle)  {
  
  ggplot(data, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title=title, subtitle=subtitle)
}

sum_something <- function(data, group_var, var) {
  data %>% 
    group_by(!!enexpr(group_var)) %>% 
    summarise(n = sum(!!enexpr(var)))
}

#función para graficos que contenga el año
gapminder_list <- split(gapminder,gapminder$year)

plot_with_purrr <- function(tablas) {
  imap(tablas, ~ {
    table <- sum_something(.x, continent, pop)
    title <- "Población mundial, según continente"
    subtitle <-paste("Año", .y)
    plot_table(table, continent, n, title,subtitle)
  })
} 

plots <- plot_with_purrr(gapminder_list)

print(plots)


##### Ejercicio 3


nested_map <- function(v1, v2) {
  map(v1,~map(v2,~print(paste(.x,.))))
  
}

nested_map(1:3, 5:8)



#QUEDA PENDIENTE EL BONUS


