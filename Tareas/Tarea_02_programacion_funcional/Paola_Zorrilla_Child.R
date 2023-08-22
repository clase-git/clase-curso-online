library(gapminder)
library(dplyr)
library(ggplot2)
library(purrr)


#Funciones dadas que se utilizan

sum_something <- function(data, group_var, var) {
  data %>% 
    group_by(!!enexpr(group_var)) %>% 
    summarise(n = sum(!!enexpr(var)))
}

plot_table <- function(table, x_var, y_var,  input_title ) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title = input_title)
}

gapminder_list <- split(gapminder, gapminder$year)

plot_with_for <- function(tablas){
  plots <- list(vector(length = length(tablas) ))
  i <- 1
  for (plot in tablas) {
    table <- sum_something(plot, continent, pop)
    plots[[i]] <- plot_table(table, continent, n, paste("Población mundial, según continente. Año", plot$year[1] )  )
    i <-  i + 1
  }
  return(plots)
}

plots <- plot_with_for(gapminder_list)   



# RESOLUCIÓN EJERCICIO 1 ----

plot_with_purrr <- function(tablas) {
  purrr::imap(tablas, ~ {
    table <- sum_something(.x, continent, pop)
    plot_table(table, continent, n, paste("Población mundial, según continente. Año", .y))
  })
}

plots_purrr <- plot_with_purrr(gapminder_list)

#Ejemplo de comparación de graficos

plots_purrr[[3]]
plots[[3]]



# RESOLUCIÓN EJERCICIO 2 ----


# Modificación de plot_table

plot_table_mod <- function(table, x_var, y_var,  input_title, input_subtitle ) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title = input_title, subtitle = input_subtitle)+
    theme(plot.title = element_text(size = 14), plot.subtitle = element_text(size = 14))
}

plot_with_purrr2 <- function(tablas) {
  plots <- purrr::imap(tablas, ~ {
    table <- sum_something(.x, continent, pop)
    plot_table_mod(table, continent, n, "Población mundial, según continente", paste("Año", .x$year[1]))
  })
  
  return(plots)
}

plots2 <- plot_with_purrr2(gapminder_list)

# Ejemplo de como queda
plots2[[2]]


## RESOLUCION EJERCICIO 3 ----

#Función a replicar

nested_for <- function(v1, v2) {
  for (x in v1) {
    for (y in v2){
      print(paste(x, y))
    }
  }
}
nested_for(1:3, 5:8) 


#Función replicada

nested_map <- function(v1, v2) {
  result <- map(v1, function(x) {
    map(v2, function(y) {
      paste(x, y)
    })
  }) %>% 
    unlist(recursive = FALSE)
    invisible(lapply(result, print))
}

nested_map(1:3, 5:8)

# La utilización del paquete purrr es más poderosa para tareas iterativas y funcionales, su utilidad es más beneficiosa en escenarios complejos o 
# cuando se trabaja con datos más anidadas. Para tareas simples y directas, como generar pares de elementos y mostrarlos, a veces complica la programación
# y genera programación con más lineas.





