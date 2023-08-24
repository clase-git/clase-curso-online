library(purrr)
library(ggplot2)
#install.packages("gapminder")
library(gapminder)

gapminder_list <- split(gapminder, gapminder$year)

sum_something <- function(data, group_var, value_var) {
  data %>%
    group_by({{ group_var }}) %>%
    summarise(total = sum({{ value_var }}))
}

plot_table <- function(data, x_var, y_var, title) {
  ggplot(data, aes({{ x_var }}, {{ y_var }})) +
    geom_bar(stat = "identity") +
    labs(title = title)
}

plot_with_purrr <- function(tablas) {
  tablas %>%
    imap(~ sum_something(.x, continent, pop) %>%
           plot_table(continent, total, paste("Población mundial, según continente. Año", .y)))
}

plots <- plot_with_purrr(gapminder_list)
plots

#Ejercicio2

plot_table <- function(data, x_var, y_var, subtitulo) {
  ggplot(data, aes({{ x_var }}, {{ y_var }})) +
    geom_bar(stat = "identity") +
    labs(subtitle = subtitulo)
}

plot_with_purrr <- function(tablas) {
  tablas %>%
    imap(~ {
      year <- .y
      table <- sum_something(.x, continent, pop)
      plot_table(table, continent, total, paste("Año", year))
    })
}

plots <- plot_with_purrr(gapminder_list)
plots


#Ejercicio 3

nested_map <- function(v1, v2) {
  map(v1, function(x) {
    map(v2, function(y) {
      paste(x, y, sep = "-")
    })
  })
}

v1 <- c(1, 2, 3)
v2 <- c(5, 6, 7, 8)

result <- nested_map(v1, v2)

# Imprimir la matriz de pares generada
for (i in seq_along(result)) {
  for (j in seq_along(result[[i]])) {
    cat(result[[i]][[j]], "\n")
  }
}

