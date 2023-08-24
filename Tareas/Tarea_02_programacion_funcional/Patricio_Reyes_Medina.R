
# Ejercicio 1 ---------------------------------------------------------

plot_with_purrr <- function(tablas) {
  plots <- purrr::map(tablas, ~ {
    table <- sum_something(.x, continent, pop)
    plot_table(
      table, 
      continent, 
      n, 
      paste("Población mundial, según continente. Año", .x$year[1]))
  })
  return(plots)
}

# Ejercicio 2 ----------------------------------------------------------

plot_table <- function(table, x_var, y_var, input_title, input_subtitle) {
  ggplot2::ggplot(table, aes(x = {{x_var}}, y = {{y_var}})) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(
      title = input_title,
      subtitle = input_subtitle)
}


plot_with_purrr <- function(tablas) {
  plots <- purrr::map(tablas, ~ {
    table <- sum_something(.x, continent, pop)
    plot_table(
      table, 
      continent, 
      n, 
      "Población mundial, según continente",
      .x$year[1])
  })
  return(plots)
}

# Ejercicio 3 --------------------------------------------------------------

nested_map <- function(v1, v2) {
  list(v1, v2) |> 
    purrr::cross() |>
    purrr::map(purrr::lift(paste)) |>
    purrr::walk(print)
}











