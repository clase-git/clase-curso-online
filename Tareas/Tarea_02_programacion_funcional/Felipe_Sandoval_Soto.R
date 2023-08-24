#####Tarea 2 Felipe Sandoval

library(gapminder)
library(tidyverse)

##### Ejercicio 1
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

plot_with_purrr <- function(tablas) {
  plots <- map(tablas, ~ {
    table <- sum_something(.x, continent, pop)
    plot_table(table, continent, n, paste("Población mundial, según continente. Año", .x$year[1]))
    })
  return(plots)
}
  
plots <- plot_with_purrr(gapminder_list)  

##### Ejercicio 2  

plot_table2 <- function(table, x_var, y_var,  input_title, input_subtitle ) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title = input_title, subtitle = input_subtitle)
}

plot_with_purrr2 <- function(tablas) {
  plots <- map(tablas, ~ {
    table <- sum_something(.x, continent, pop)
    plot_table2(table, continent, n,"Población mundial, según continente.",paste("Año",.x$year[1]))
  })
  return(plots)
}

plots2 <- plot_with_purrr2(gapminder_list) 

##### Ejercicio 3

nested_for <- function(v1, v2) {
    for (x in v1) {
      for (y in v2){
        print(paste(x, y))
      }
    }
  }

nested_purrr <- function(v1, v2) {
  v<-map(v1,~paste(.x,.y),v2)
  v<-map(v,print)
}

nested_map <- function(v1, v2) {
  v<-expand_grid(v1,v2) %>% pmap_chr(paste)
  v<-map(v,print)
}

nested_for(1:3, 5:8) 
nested_purrr(1:3, 5:8)
nested_map(1:3, 5:8) 

