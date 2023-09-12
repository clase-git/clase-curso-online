### Funciones

if(!require("dplyr")){install.packages("dplyr")}
if(!require("purrr")){install.packages("purrr")}
if(!require("stringr")){install.packages("stringr")}
if(!require("data.table")){install.packages("data.table")}
if(!require("fs")){install.packages("fs")}
if(!require("readr")){install.packages("readr")}
if(!require("janitor")){install.packages("janitor")}
if(!require("survey")){install.packages("survey")}
if(!require("microbenchmark")){install.packages("microbenchmark")}

## función para extraer nombres de la url en formato csv
extract_name <- function(url){
    names<-purrr::map_chr(url, ~{
    paste0(stringr::str_extract(.x,pattern="[a-z]{3}-[0-9]{4}---"),"personas.csv")
    }) ## se extrae el patron del tipo "esi-aaaa---" donde aaaa corresponde al año
}

## función para descargar todos los archivos de la url
download_esi_data <- function(url,names,directory){
  if (fs::is_dir(directory)==F) {fs::dir_create(directory)} ##revisa si existe una carpeta con el nombre especificado en "directory", si no la crea
  directorio <- paste0(directory,"/",names)  ##se crean los directorios de los archivos a guadar
  #directorio<-na.omit(purrr::map_chr(directorio,~{case_when(fs::is_file(.x)==F~.x,TRUE~NA)})) ## borra los directorios de los archivos que ya existen
  directorio<-na.omit(purrr::map_chr(directorio,~{ifelse(fs::is_file(.x)==F,.x,NA)})) ## por alguna razón, case_when no funcionó en otro computador cuando lo probé
  for (i in 1:length(url) ) {if (i %in% attr(directorio,"na.action")) {url[i]<-NA}} ## convierte a NA las url de los archivos que ya existen
  url<-na.omit(url) ## borra los NA del vector de urls
  files <- purrr::map(url, ~{data.table::fread(.x)}) ## se leen los archivos y se guardan en lista usando map
  purrr::walk2(files,directorio,data.table::fwrite)## se escriben los archivos de la lista con los directorios que se habían guardado
  if (length(url)==0) {print("Todos los archivos ya existen")}
} ## esta función sólo descargará los archivos que no existan en la carpeta especificada.

## función para leer solo un archivo
read_esi_data <- function(ruta,decimal="."){
  v<-data.table::fread(ruta,dec = decimal)
  v[,version:=paste0("esi_",stringr::str_extract(ruta,pattern="[0-9]{4}"))]
  v<-janitor::clean_names(v)
} ## para esta función se pidió que la función reconozca el separado y leer el archivo correctamente en todos los casos, 
## fread de data.table es muy eficiente en ese sentido, ahora bien, quise agregar un separador decimal en caso de que
## el decimal fuera coma en lugar de punto, eso si, la función lo necesita sólo si se especifica.
## se le agrega además la versión de la esi para usarla en el próximo ejercicio, para que dependa del nombre del csv y no del objeto creado
  
## función que devuelve cantidad de personas y hogares por versión
tabla_pers_hog <- function(archivo){
  a<-archivo[,.N,by=idrph][,.N,] ## se cuentan las personas
  b<-archivo[,.N,by=id_identificacion][,.N,] ## se cuentan las viviendas
  c<-archivo$version[1] ## se extrae la versión del archivo
  data.table(version=c,n_personas=a,n_hogares=b) ## crea el archivo en formato de tabla
}  

## Aquí hice una función que saca todo lo pedido, pero por versión y no por hogar
## se utiliza funciones de la librería data.table
tabla_fact_cal<- function(archivo){
  a<-archivo[,.SD[which.min(fact_cal_esi)],by=.(version)
             ][,.(min=fact_cal_esi),by=version]
  b<-archivo[,.SD[which.max(fact_cal_esi)],by=.(version)
             ][,.(max=fact_cal_esi),by=version]
  c<-archivo[,.SD[mean(fact_cal_esi)],by=.(version)
             ][,.(media=fact_cal_esi),by=version]
  d<-archivo[,.SD[median(fact_cal_esi)],by=.(version)
             ][,.(mediana=fact_cal_esi),by=version]
  e<-archivo[,.SD[quantile(fact_cal_esi, probs = 0.10)
                  ],by=.(version)][,.(p10=fact_cal_esi),by=version]
  f<-archivo[,.SD[quantile(fact_cal_esi, probs = 0.90)
                  ],by=.(version)][,.(p90=fact_cal_esi),by=version]
  a[b[c[d[e[f,on="version"],on="version"],on="version"],on="version"],on="version"]
} 

## función que cuenta la cantidad de estratos que tienen sólo un conglomerado
tabla_est_con <- function(archivo){
  archivo[,est_con := .N,by=.(estrato,conglomerado,version) ## se suman los conglomerados distintos por estrato
          ][est_con==1  ## sólo consideramos los estratos con un conglomerado
            ][,.N,by=version] ## contamos 
}

## función que calcula lo pedido a nivel de personas, por lo mismo se debe usar el paquete
## survey para datos complejos, ya que si bien no es dificil tener el máximo, el mínimo
## o el promedio, la cosa se complica al tener que calcular los quantiles
tabla_ingreso <- function(archivo, reg = 17){
  if (reg!=17) {archivo <- dplyr::filter(archivo, region==reg)}
  disenio<-survey::svydesign(id = ~conglomerado, strata = ~estrato, weights = ~fact_cal_esi, data = archivo) 
           options(survey.lonely.psu="remove")
  min <- min(disenio[["variables"]][["ing_t_p"]])
  max <- max(disenio[["variables"]][["ing_t_p"]])
  media <- survey::svymean(~ing_t_p, disenio, na.rm = TRUE, covmat = TRUE)[1]
  mediana <- survey::svyquantile(~ing_t_p, disenio, 0.5)[["ing_t_p"]][1]
  p10 <- survey::svyquantile(~ing_t_p, disenio, 0.1)[["ing_t_p"]][1]
  p90 <- survey::svyquantile(~ing_t_p, disenio, 0.9)[["ing_t_p"]][1]
  data.table(version=archivo$version[1],min=min,max=max,media=media,mediana=mediana, p10=p10, p90=p90)
}


## función que calcula sólo el ingreso medio con data.table, se puede filtrar por región
ingreso_medio <- function(archivo, reg = 17){
  if (reg!=17) {archivo <- dplyr::filter(archivo, region==reg)}
 archivo[,ingreso:=fact_cal_esi*ing_t_p,
         ][,sum(ingreso)/sum(fact_cal_esi),]
}


