###Ejercicio 1###

library(gapminder)
library(dplyr)
library(purrr)
library(ggplot2)

gapminder_list <- split(gapminder, gapminder$year)

sum_something <- function(gapminder_data, continent_var, pop_var) {
  gapminder_data %>%
    group_by(!!sym(continent_var)) %>%
    summarise(n = sum(!!sym(pop_var)))
}

tablas <- map(gapminder_list, sum_something, continent_var = "continent", pop_var = "pop")

plot_with_purrr <- function(tablas) {
  plots <- tablas %>%
    map(~ ggplot(data = ., aes(x = continent, y = n, fill = continent)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = paste(round(n / 1e6, 2), "M")), vjust = -0.5)+
          labs(title = paste("Población mundial, según continente. Año", .$year[1]))
    )
  return(plots)
}

plots <- plot_with_purrr(tablas)
plots

###Ejercicio 2###

library(gapminder)
library(dplyr)
library(purrr)
library(ggplot2)

gapminder_list <- split(gapminder, gapminder$year)

sum_something <- function(gapminder_data, continent_var, pop_var) {
  gapminder_data %>%
    group_by(!!sym(continent_var)) %>%
    summarise(n = sum(!!sym(pop_var)))
}

tablas <- map(gapminder_list, sum_something, continent_var = "continent", pop_var = "pop")

plot_with_purrr <- function(tablas) {
  plots <- tablas %>%
    map2(names(tablas), ~ {
      ggplot(data = .x, aes(x = continent, y = n, fill = continent)) +
        geom_bar(stat = "identity") +
        labs(title = "Población mundial por continente",
             subtitle = paste("Año:", .y),
             y = "Población (millones)", x = "Continente") +
        geom_text(aes(label = paste(round(n / 1e6, 2), "M")), vjust = -0.5)
    })
  return(plots)
}

plots <- plot_with_purrr(tablas)
plots

###Ejercicio 3###

library(purrr)

nested_purrr <- function(v1, v2) {
  v1=1:3
  v2=5:8
  pairs <- expand.grid(v1 = v1, v2 = v2)
  map2(pairs$v1, pairs$v2, ~ paste(.x, .y))
}

result <- nested_purrr(1:3, 5:8)
print(result)

# Se considera que el paquete purrr (función map), posee una ventaja 
# para el tratamiento de objetos de mayor dimensión. 
# No aplica una mayor ventaja en el ejercicio antertior.

