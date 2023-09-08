# functions.R

calcular_promedio <- function(mtcars) {
  # Cálculo del promedio
  
  promedio <- mean(mtcars$hp)
  return(promedio)
}

