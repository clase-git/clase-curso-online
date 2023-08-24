#cargamos las librerias a utilizar
library(gapminder)
library(dplyr)
library(ggplot2)
library(rlang)
library(purrr)

# EJERCICIO 1 -------------------------------------------------------------
#separamos la base de datos en en distintas listas, filtradas por año
gapminder_list <- split(gapminder, gapminder$year)

### cargamos la función que suma la información por grupo
sum_something <- function(data, group_var, var) {
  data %>% 
    group_by(!!enexpr(group_var)) %>% 
    summarise(n = sum(!!enexpr(var)))
}
## cargamos la función que crea un gráfico de barras, en el que agregamos el título
plot_table <- function(table, x_var, y_var, input_title) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var))) +
    geom_bar(stat = "identity") +
    labs(title = input_title)
}
## cargamos la función que nos creara un gráfico (plot_table)por año del resumen obtenido por
## función sum_something()

plot_with_purrr <- function(tablas) {
  plots <- map(tablas, ~ {
    table <- sum_something(.x, continent, pop)
    plot_table(table, continent, n, paste("Población mundial, según continente. Año", .x$year[1]))
  })
  return(plots)
}

##cargamos los graficos por continte y por por cada año.
plots <- plot_with_purrr(gapminder_list)


# el mismo procedimiento con IMAP -----------------------------------------
sum_something <- function(data, group_var, var) {
  data %>% 
    group_by(!!enexpr(group_var)) %>% 
    summarise(n = sum(!!enexpr(var)))
}

plot_table <- function(table, x_var, y_var, input_title) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var))) +
    geom_bar(stat = "identity") +
    labs(title = input_title)
}

plot_with_purrr_imap <- function(tablas) {
  plots <- tablas %>%
    imap(~ {
      table <- sum_something(.x, continent, pop)
      plot_table(table, continent, n, paste("Población mundial, según continente. Año", .x$year[1]))
    })
  return(plots)
}

gapminder_list <- split(gapminder, gapminder$year)
plots <- plot_with_purrr_imap(gapminder_list)



# EJERCICIO 2 -------------------------------------------------------------

sum_something <- function(data, group_var, var) {
  data %>% 
    group_by(!!enexpr(group_var)) %>% 
    summarise(n = sum(!!enexpr(var)))
}
### Modificamos la función plot_table
### agregando un nuevo parámetro a la función "input_subtitle"
### 
plot_table <- function(table, x_var, y_var, input_title,input_subtitle) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var))) +
    geom_bar(stat = "identity") +
    labs(title = input_title, subtitle = input_subtitle) ##agregamos subtitle
}

plot_with_purrr <- function(tablas) {
  plots <- map(tablas, ~ {
    table <- sum_something(.x, continent, pop)
    plot_table(table, continent, n, paste("Población mundial, según continente"),paste("Año",.x$year[1]))
  })## agregamos el nuevo parámetro que es año para el subtítulo
  return(plots)
}

gapminder_list <- split(gapminder, gapminder$year)
plots <- plot_with_purrr(gapminder_list)



# EJERCICIO 3 -------------------------------------------------------------
# función recibe 2 vectores, lo transforma en lista y 
#realiza las combinaciones entre los vectores 
library(tidyr)
nested_map <- function(v1, v2) {
  data <- list( v1, v2 )
  walk(data %>%
         cross() %>%
         map(lift(paste)), print)
}

nested_map(6:7, 10:12)

##para este tipo de ejercicios es mejor utilizar la programación con for, que
#la utilización de la libreria purrr, ya que esta es para tareas iterativas con datos 
#anidados y realizar este tipo de funciones se nos puede complicar.

