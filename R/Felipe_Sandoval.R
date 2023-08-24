##Prueba - Felipe Sandoval
##Agregando cambios

##codigo sesion 1
library(dplyr)
library(tidyverse)

df <- dplyr::tibble(x=list(1,2:3,4:6))
df %>% dplyr::mutate(largo = map_int(x,length))
df %>% rowwise() %>% mutate(largo=length(x))


