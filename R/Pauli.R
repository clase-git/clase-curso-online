library(dplyr)

mtcars %>%
  group_by(gear)
#rowwise, trabaja con filas para operar con ellas

#c_across atraves de columnas, starts_with

mtcars2<-mtcars %>%
  rowwise()%>% #operaciones fila con fila, ayuda hacer loop
  mutate(suma=sum(c_across(where(is.numeric))))

#tibble columnas
#tribble filas

df<-
  dplyr:: tibble(
  x<-list(1,2:3,4:6)
)

df |>
  dplyr::rowwise() |>
   dplyr::mutate(largo=length(x))

#rowSums o rowMeans


library(dplyr)



df <- dplyr::tibble(x=list(1,2:3,4:6))



df %>% 
  mutate(largo = length(x))



df %>% 
  dplyr:rowwise() %>% 
  mutate(largo = length(x))



df <- tribble(
  ~ n, ~ min, ~ max,
  4,     0,     1,
  2,    10,   100,
  3,   100,  1000,
)



df2 <- df %>% 
  mutate(data = runif(n, min, max))
df2



df2 <- df %>% 
  rowwise() %>% 
  mutate(data = list(runif(n, min, max)))
df2$data



mtcars2 <- mtcars
for (var in names(mtcars)) {
  mtcars2[[var]] <- (mtcars[[var]] - min(mtcars[[var]])) / (max(mtcars[[var]]) - min(mtcars[[var]]))
}



mtcars2 <- mtcars %>% 
  dplyr::mutate(
    dplyr::across(
      .cols=dplyr::everything(),
      .fns = ~ (.x - min(.x)) / (max(.x) - min(.x)) ) )



mtcars2 <- mtcars %>% 
  dplyr::mutate(
    dplyr::across(
      .cols=c(mpg,cyl),
      .fns = ~ (.x - min(.x)) / (max(.x) - min(.x)) ) )


mtcars2 <- mtcars %>% 
  dplyr::mutate(
    dplyr::across(
      .cols=c(mpg,disp),
      .fns = list(
        norm=~ (.x - min(.x)) / (max(.x) - min(.x)) ) ),
    .names="{.fn}_{col}"
      )
ungroup()

do_silly_stuff <- function(x) {
  normalizar <-  (x - min(x)) / (max(x) - min(x))
  norm_media <-  normalizar + mean(normalizar)
  norm_mediana <- norm_media / median(norm_media)
  return(norm_mediana)
}
mtcars2 <- mtcars %>% 
  mutate(across(c("mpg", "disp"),
                .fns = list(norm = ~do_silly_stuff(.))  ))