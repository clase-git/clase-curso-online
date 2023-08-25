
### TAREA ####
#### EJERCICIO 1 ##########

## Considero las funciones sum_something y plop_table 

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

#Lista
gapminder_list <- split(gapminder, gapminder$year)

#Funcion
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
plots


#La tarea consiste en llegar al mismo resultado, pero utilizando únicamente las herramientas de purrr. Crea una función llamada plot_with_purrr que reciba una lista de tablas y devuelva una lista de gráficos
#Pista: La función imap puede ser de gran ayuda

plot_with_purrr <- function(tablas) {
  plots <- imap(tablas, function(plot, i) {
    table <- sum_something(plot, continent, pop)
    plot_table(table, continent, n, paste("Población mundial, según continente. Año", plot$year[1]))
  })
  return(plots)
}

plots <- plot_with_purrr(gapminder_list)



## Ejercicio 2

#Modifica la función plot_table para que el año del gráfico aparezca en el subtítulo y no en el título. 
#La función modificada debería recibir un parámetro extra llamado subtitulo, que permita agregar el año al subtítulo del gráfico.


## Se modifica la funcion plot_table 
plot_table <- function(table, x_var, y_var,  input_title, input_subtitle ) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title = input_title) +
    labs(subtitle = paste("Año:", input_subtitle))
}


#Se agrega a la anterior función plot_with_purrr
plot_with_purrr <- function(tablas) {
  plots <- imap(tablas, function(plot, i) {
    table <- sum_something(plot, continent, pop)
    plot_table(table, continent, n, paste("Población mundial, según continente"), plot$year[1])
  })
  return(plots)
}


gapminder_list <- split(gapminder, gapminder$year)
plots_purrr <- plot_with_purrr(gapminder_list)
plots_purrr

##Ejercicio 3

## El siguiente for anidado genera pares de x e y. 
## El ejercicio consiste en escribir una función llamada nested_map que utilice una sintaxis de purrr. 
## La función debe recibir dos vectores numéricos (de igual o distinto largo) e imprimir pares de número.
## Es posible que la sintaxis llegue a ser un poco confusa. Reflexiona sobre la pertinencia de purrr para tareas de este tipo.


nested_for <- function(v1, v2) {
  for (x in v1) {
    for (y in v2){
      print(paste(x, y))
    }
  }
}

nested_for(1:3, 5:8)


#Se crea una función del paquete purrr, usando la funcion map 
nested_map <- function(v1, v2) {
  map(v1, function(x) {
    map(v2, function(y) {
      print(paste(x, y))
    })
  })
}

nested_map(1:3, 5:8)


## Dado que el objetivo de la funcion no es complejo y considera poca cantidad de argumentos, 
# se considera que no es necesaria la aplicación de purrr en este tipo de funciones, ya que el bucle for soluciona bien la problemática.







