
# Ejercicio 1 ##############
# El siguiente código genera un resultado muy similar al del último ejercicio revisado en la clase. La diferencia es que la implementación es mediante un ciclo for.
# Adicionalmente, se agrega una funcionalidad que agrega al título el año correspondiente.

library(gapminder)
library(dplyr)
library(ggplot2)
library(purrr)

### funciones creadas en ppt de curso y utilizadas para este ejercicio
sum_something <- function(data, group_var, var) {
  data %>% 
    group_by(!!enexpr(group_var)) %>% 
    summarise(n = sum(!!enexpr(var)))
}

plot_table <- function(table, x_var, y_var, input_title ) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title = input_title)
}

gapminder_list <- split(gapminder, gapminder$year)   #### lista de insumo para utilizar en función de gráficos

#### función que genera lista de gráficos que se debe replicar con purr
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

plots <- plot_with_for(gapminder_list)  ### resultado a replicar
plots[2]

# La tarea consiste en llegar al mismo resultado, pero utilizando únicamente las herramientas de purrr. Crea una función llamada plot_with_purrr que reciba una lista de tablas 
# y devuelva una lista de gráficos

############## Resolución ejercicio ##################

#### función que genera lista de gráficos usando comandos de purr
plot_with_purrr <- function(tablas){
  table <- imap(tablas, ~ sum_something(.x, continent, pop))
  graficos <- map2(table, paste("Población mundial, según continente. Año ", names(table)), ~ plot_table(.x, continent, n, .y))
  return(graficos) 
}

# Ejemplos
graficos <- plot_with_purrr(gapminder_list)
graficos[7]
graficos[11]

# Ejercicio 2 ##############
# 
# Modifica la función plot_table para que el año del gráfico aparezca en el subtítulo y no en el título. 
# La función modificada debería recibir un parámetro extra llamado subtitulo, que permita agregar el año al subtítulo del gráfico.
# Una vez que hayas modificado tu función, utilízala dentro de plot_with_purrr. 
# Cada gráfico debería tener el año correspondiente en el subtítulo.

############## Resolución ejercicio ##################

# Función modificada plot table
plot_table2 <- function(table, x_var, y_var, input_title, subtitulo) {
  ggplot(table, aes(x = !!enexpr(x_var), y = !!enexpr(y_var) )) +
    geom_bar(stat = "identity") +
    labs(title = input_title) +
    labs(subtitle = subtitulo)
}

# Ajuste a función plot_whit_purr para agregar subtìtulo
plot_with_purrr2 <- function(tablas){
  table <- imap(tablas, ~ sum_something(.x, continent, pop))
  graficos <- map2(table, names(table), ~ plot_table2(.x, continent, n, "Población mundial, según continente. Año ", .y))
  return(graficos) 
}

# Ejemplos
graficos <- plot_with_purrr2(gapminder_list)
graficos[8]
graficos[12]

# Ejercicio 3 ##############
# 
# El siguiente for anidado genera pares de x e y. El ejercicio consiste en escribir una función llamada nested_map 
# que utilice una sintaxis de purrr. La función debe recibir dos vectores numéricos (de igual o distinto largo) e imprimir pares de número.
# Es posible que la sintaxis llegue a ser un poco confusa. Reflexiona sobre la pertinencia de purrr para tareas de este tipo.

nested_for <- function(v1, v2) {
  for (x in v1) {
    for (y in v2){
      print(paste(x, y))
    }
  }
}

nested_for(1:3, 5:8)

############## Resolución ejercicio ##################
nested_map <- function(v1, v2) {
  combinaciones <- expand.grid(v1 = v1, v2 = v2)  # con función expand.grid se crean las combinaciones posibles
  pmap(combinaciones, ~ print(paste(.x, .y)))  
}

# Ejemplos
nested_map(1:3, 5:8)
nested_map(2:5, 6:9)

# Bonus ##############

# Si quieres practicar más, puedes realizar el siguiente ejercicio. No es parte de la evaluación, 
# pero te puede dar puntos extras en caso de que no obtengas todo el puntaje en alguno de los ejercicios anteriores.

# Con los datos de la ENE y siempre utilizando purrr, realiza las siguientes tareas:

# Carga todos los trimestres de 2022 en una lista
# Escribe una función llamada get_employment_sample que reciba un dataframe y 
# devuelva la cantidad de ocupados sin expandir. La categoría 1 de la variable activ muestra a los ocupados.
# Usando get_employment_sample, genera un gráfico que muestre la cantidad de ocupados y no ocupados para cada uno de los trimestres.
# Guarda cada uno de los gráficos en una carpeta de tu computador en formato jpg o pdf.
# Abre un rmd y carga todos los gráficos guardados (usando purrr)
# Genera un pdf o html que tenga en su interior los gráficos
# Para los últimos 2 pasos, deberás empujar al repositorio, además, el archivo rmd

# Define la ubicación de las bases
ruta <- "C:/AARossana/Boletín Empleo/Bases ENE-trimestrales"

# # lista de bases .RData del año 2022 
bases_2022 <-  list.files(ruta, pattern ="^2022-[0-9]+\\.RData$", full.names = TRUE)

# Función para cargar bases de datos 
cargar_base <-  function(archivo)  {
  contenido <-  get(load(archivo))
  return(contenido)
}

# Cargar bases de datos del año 2022 en una lista 
lista_bases_2022 <-  map(bases_2022, ~ cargar_base(.))

#### creamos función para crear variable de ocupados y no ocupados y encontrar cantidad 
get_employment_sample <- function(data){
  ocupados <- sum(data$activ==1, na.rm=TRUE)
  no_ocupados <- sum(data$activ != 1, na.rm=TRUE)
  return(data.frame(Ocupados = ocupados, NoOcupados= no_ocupados))
}

#### obtenemos la lista del total ocupados y no ocupados para cada base utilizando la función get_employment_sample
resultados <- map(lista_bases_2022, get_employment_sample)

ruta_completa <- "C:/AARossana/Cursos y capacitaciones/2023/Curso R Intermedio/graficos/"  
getwd()

crearYGuardarGrafico <-  function(resultados, nombre_archivo)  {
  gg <-  ggplot(resultados,  aes(x = c("Ocupados", "No Ocupados"), y = c(Ocupados, NoOcupados))) +
    geom_bar(stat  = "identity")  +
    labs(title ="Distribución de Ocupados y No Ocupados",  y = "Cantidad", x = "Estado")  +
    theme_minimal()   
  ggsave(nombre_archivo, plot = gg, width = 6, height = 4,  dpi = 300)
}

# Aplicar la función para crear y guardar los gráficos a cada base de datos 
map2(names(lista_bases_2022), resultados, ~ crearYGuardarGrafico(.y, paste(.x, ".jpg", sep = "")))

# Hasta acá llegué :( no alcancè a hacer el archivo rmd
