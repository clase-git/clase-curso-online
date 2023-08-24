library(dplyr)
library(gapminder)
library(tidyverse)
library(rlang)
library(purrr)
library(feather)

plot_table <- function(table, x_var, y_var,  input_title ) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title = input_title)
}

sum_something <- function(data, group_var, var) {
  data %>%
    group_by(!!enexpr(group_var)) %>%
    summarise(n = sum(!!enexpr(var)))
}

gapminder_list <- split(gapminder, gapminder$year)
plot_with_for <- function(tablas){
  plots <- list(vector(length = length(tablas) ))
  i <- 1
  for (plot in tablas) {
    table <- sum_something(plot, continent, pop)
    plots[[i]] <- plot_table(table, continent, n, paste("Población mundial, según continente. Año", plot$year[1] )  )
     i <- i + 1
  }
  return(plots)
}
plots <- plot_with_for(gapminder_list)



#Ejercicio 1

plot_with_purrr <- function(tablas) {
  imap(tablas, ~ {
    table <- sum_something(.x, continent, pop)
    input_title <- paste("Población mundial, según continente. Año", .y)
    plot_table(table, continent, n, input_title)
  })
}

plots <- plot_with_purrr(gapminder_list)

print(plots)



#Ejercicio 2

# Función para crear el gráfico con subtítulo
plot_table <- function(table, x_var, y_var, title, subtitulo) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var))) +
    geom_bar(stat = "identity") +
    labs(title = title, subtitle = subtitulo)
}

sum_something <- function(data, group_var, var) {
  data %>%
    group_by(!!enexpr(group_var)) %>%
    summarise(n = sum(!!enexpr(var)))
}

gapminder_list <- split(gapminder, gapminder$year)

plot_with_purrr <- function(tablas) {
  imap(tablas, ~ {
    table <- sum_something(.x, continent, pop)
    title <- "Población mundial según continente"
    subtitulo <- paste("Año", .y)
    plot_table(table, continent, n, title, subtitulo)
  })
}

plots <- plot_with_purrr(gapminder_list)

print(plots)

#Ejercicio 3

nested_for <- function(v1, v2) {
  for (x in v1) {
    for (y in v2){
      print(paste(x, y))
    }
  }
}
nested_for(1:3, 5:8)

library(purrr)

nested_map <- function(v1, v2) {
  map(v1, ~ map(v2, ~ print(paste(.x, .))))
}

nested_map(1:3, 5:8)



