library(sf)
library(ggplot2)
library(tidyverse)
library(tmap)

# Limpiar ambiente
rm(list=ls())

#
tmap_mode("view")

### Informaciónn de Distritos Electorales

## Distritos locales

# archivos temporales
temp <- tempfile()
temp2 <- tempfile()
# descarga el archivo .shp y se guarda en 'temp' 
download.file("https://cartografia.ife.org.mx//descargas/distritacion2017/local/06/06.zip",temp)
#descomprimir y guardar en 'temp2'
unzip(zipfile = temp, exdir = temp2)
# encontrar ruta del shapefile (.shp) en temp2
# el $ al final de ".shp$" asegura de no encontrar archivos como: .shp.xml 
dist_local <- list.files(temp2, pattern = ".shp$",full.names=TRUE)

# Leer el archivo y guardarlo en variable
dist_local <- read_sf(dist_local)
levels_dis <- as.numeric(order(unique(c(dist_local$DISTRITO_L)), decreasing = FALSE))
dist_local$DISTRITO_L <- factor(dist_local$DISTRITO_L, levels_dis)

dist_local <- st_transform(dist_local, 4326)
st_write(dist_local, "data/shp/dist_local.shp", append = FALSE)

## Distritos federales (para obtener secciones)

temp <- tempfile()
temp2 <- tempfile()
download.file("https://cartografia.ife.org.mx//descargas/distritacion2017/federal/06/06.zip",temp)
unzip(zipfile = temp, exdir = temp2)

dist_federal <- list.files(temp2, pattern = ".shp$",full.names=TRUE)

secciones <- read_sf(dist_federal[4])
secciones <- st_transform(secciones, 4326)
st_write(secciones, "data/shp/secciones.shp", append = FALSE)

# Join de secciones en distrito local 3

join <- st_join(secciones, dist_local, left = TRUE, join = st_intersects, largest = TRUE)

distrito <- join %>% filter(DISTRITO_L == 3)
levels_secc <- c(distrito$seccion)
distrito$seccion <- factor(distrito$seccion, levels_secc)

distrito <- st_transform(distrito, 4326)
st_write(distrito, "data/shp/distrito.shp", append = FALSE)

tm_shape(distrito) + 
  tm_polygons(col = "seccion")+
  tm_shape(dist_local)+
  tm_polygons(alpha = 0.5)

# Capas de geoestadística
# descargar y descomprimir, viene de 

temp <- tempfile()
temp2 <- tempfile()
download.file("https://www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/889463807469/06_colima.zip",temp)
unzip(zipfile = temp, exdir = temp2)

ar <- read_sf(paste0(temp2,"/conjunto_de_datos/06ar.shp"))
au <- read_sf(paste0(temp2,"/conjunto_de_datos/06a.shp"))
mz <- read_sf(paste0(temp2,"/conjunto_de_datos/06m.shp"))

au <- select(au, c(-"CVE_LOC"))

ar <- st_transform(ar, 4326)
au <- st_transform(au, 4326)
au <- st_transform(au, 4326)
mz <- st_transform(mz, 4326)

ageb <- rbind(ar, au)

st_write(ageb, "data/shp/ageb.shp", append = FALSE)
st_write(mz, "data/shp/mz.shp", append = FALSE)

distrito_tres <- dist_local %>%
  filter(DISTRITO_L == 3)

rm(ar, au, join, secciones, dist_local)
rm(dist_federal, levels_dis, levels_secc, temp, temp2)

mz_distritos <- st_join(mz, dist_local, left = TRUE, join = st_intersects, largest = TRUE)
mz_distritos <- mz_distritos %>%
  mutate(ID_AGEB_MZ = paste0(CVE_AGEB, "-", CVE_MZA))

mz_distritos %>%
  dplyr::filter(DISTRITO_L == 3) %>%
  tm_shape() + 
  tm_polygons(col = "CVE_AGEB", alpha = 0.5)+
  tm_shape(distrito)+
  tm_polygons(alpha = 0.3, col = "blue")

## temp <- tempfile()
## temp2 <- tempfile()
## download.file("https://www.inegi.org.mx/contenidos/programas/accidentes/datosabiertos/atus_anual_csv.zip",temp)
## unzip(zipfile = temp, exdir = temp2)
## 
## list.files(temp2, full.names=TRUE)
## acc2019 <- read_csv(paste0(temp2,"/atus_anual_1997_2019/conjunto_de_datos/atus_anual_2019.csv"))

temp <- tempfile()
temp2 <- tempfile()
download.file("https://www.inegi.org.mx/contenidos/programas/ccpv/2020/microdatos/ageb_manzana/RESAGEBURB_06_2020_csv.zip",temp)
unzip(zipfile = temp, exdir = temp2)

list.files(temp2, full.names=TRUE)
censo <- read_csv(paste0(temp2, "/RESAGEBURB_06CSV20.csv"))

censo[] <- lapply(censo, gsub, pattern='\\*', replacement='0')
censo[9:230] <- lapply(censo[9:230], as.numeric)

censo_mz <- censo %>% 
  filter(MZA != "000") %>% 
  group_by(AGEB, MZA) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)

censo_mz <- censo_mz %>%
  mutate(ID_AGEB_MZ = paste0(AGEB, "-", MZA)) %>%
  select(ID_AGEB_MZ, everything())

base_inegi <- inner_join(mz_distritos, censo_mz, by = "ID_AGEB_MZ")

st_write(base_inegi, "data/shp/base_inegi.shp", append = FALSE)

sum_inegi <- base_inegi %>%
  group_by(DISTRITO_L) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)

tm_shape(sum_inegi) + 
  tm_polygons(col = "POBTOT")

