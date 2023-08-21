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





# Definir la función plot_with_purrr para la población mundial según continente
plot_with_purrr <- function(year_list) {
  # Mapear sobre la lista de años y generar un gráfico para cada año
  plot_list <- map(year_list, function(year) {
    year_data <- gapminder[gapminder$year == year, ]
    continent_pop <- tapply(year_data$pop, year_data$continent, sum)
    pop_df <- data.frame(continent = names(continent_pop), population = continent_pop)
    
    ggplot(data = pop_df, aes(x = continent, y = population, fill = continent)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Población mundial, según continente. Año -", year),
           x = "Continente",
           y = "Población",
           fill = "Continente") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas del eje x
    # Puedes personalizar el gráfico según tus necesidades
  })
  
  return(plot_list)
}

# Obtener la lista de años únicos en la base de datos gapminder
years <- unique(gapminder$year)

# Generar la lista de gráficos utilizando la función
graficos <- plot_with_purrr(years)

# Imprimir los gráficos (puedes guardarlos o visualizarlos según tus necesidades)
print(graficos)




# Crear la lista de tablas por año
gapminder_list <- split(gapminder, gapminder$year)

# Función para generar los gráficos utilizando purrr
plot_with_purrr <- function(tablas) {
  map(tablas, function(plot) {
    table <- sum_something(plot, continent, pop)
    input_title <- paste("Población mundial, según continente. Año", plot$year[1])
    plot_table(table, continent, n, input_title)
  })
}

# Generar los gráficos utilizando la función plot_with_purrr
plots <- plot_with_purrr(gapminder_list)

# Imprimir los gráficos (puedes guardarlos o visualizarlos según tus necesidades)
print(plots)


gapminder_list <- split(gapminder, gapminder$year)

# Función para generar los gráficos utilizando purrr::imap
plot_with_purrr <- function(tablas) {
  imap(tablas, ~ {
    table <- sum_something(.x, continent, pop)
    input_title <- paste("Población mundial, según continente. Año", .y)
    plot_table(table, continent, n, input_title)
  })
}

# Generar los gráficos utilizando la función plot_with_imap
plots <- plot_with_purrr(gapminder_list)

# Imprimir los gráficos (puedes guardarlos o visualizarlos según tus necesidades)
print(plots)





# Función para crear el gráfico con subtítulo
plot_table <- function(table, x_var, y_var, title, subtitulo) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var))) +
    geom_bar(stat = "identity") +
    labs(title = title, subtitle = subtitulo)
}

# Función para resumir datos
sum_something <- function(data, group_var, var) {
  data %>%
    group_by(!!enexpr(group_var)) %>%
    summarise(n = sum(!!enexpr(var)))
}

# Crear la lista de tablas por año
gapminder_list <- split(gapminder, gapminder$year)

# Función para generar los gráficos utilizando purrr::imap
plot_with_purrr <- function(tablas) {
  imap(tablas, ~ {
    table <- sum_something(.x, continent, pop)
    title <- "Población mundial según continente"
    subtitulo <- paste("Año", .y)
    plot_table(table, continent, n, title, subtitulo)
  })
}

# Generar los gráficos utilizando la función plot_with_purrr
plots <- plot_with_purrr(gapminder_list)

# Imprimir los gráficos (puedes guardarlos o visualizarlos según tus necesidades)
print(plots)


