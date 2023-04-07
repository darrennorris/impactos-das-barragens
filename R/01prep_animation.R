library(tidyverse)
library(readxl)
library(sf)
library(raster)
library(terra)
library(tmap)

# Load files
buffers <- "data/vector/barragem_impactos.GPKG"
#sf::st_layers(buffers)
ada_buff_31976 <- sf::st_read(buffers, layer = "faixas") 
# limites 
myexent <- ext(vect(ada_buff_31976)) 
# arquivos
rin_12 <- "data/mapbiomas_AP_utm_rio/utm_cover_AP_rio_2012.tif"
rin_13 <- "data/mapbiomas_AP_utm_rio/utm_cover_AP_rio_2013.tif"
rin_14 <- "data/mapbiomas_AP_utm_rio/utm_cover_AP_rio_2014.tif"
rin_15 <- "data/mapbiomas_AP_utm_rio/utm_cover_AP_rio_2015.tif"
rin_16 <- "data/mapbiomas_AP_utm_rio/utm_cover_AP_rio_2016.tif"
rin_17 <- "data/mapbiomas_AP_utm_rio/utm_cover_AP_rio_2017.tif"
rin_18 <- "data/mapbiomas_AP_utm_rio/utm_cover_AP_rio_2018.tif"
rin_19 <- "data/mapbiomas_AP_utm_rio/utm_cover_AP_rio_2019.tif"
rin_20 <- "data/mapbiomas_AP_utm_rio/utm_cover_AP_rio_2020.tif"

mapbiomas_2012 <- rast(rin_12) %>% 
  crop(myexent, snap="out")
mapbiomas_2013 <- rast(rin_13) %>% 
  crop(myexent, snap="out")
mapbiomas_2014 <- rast(rin_14) %>% 
  crop(myexent, snap="out")
mapbiomas_2015 <- rast(rin_15) %>% 
  crop(myexent, snap="out")
mapbiomas_2016 <- rast(rin_16) %>% 
  crop(myexent, snap="out")
mapbiomas_2017 <- rast(rin_17) %>% 
  crop(myexent, snap="out")
mapbiomas_2018 <- rast(rin_18) %>% 
  crop(myexent, snap="out")
mapbiomas_2019 <- rast(rin_19) %>% 
  crop(myexent, snap="out")
mapbiomas_2020 <- rast(rin_20) %>% 
  crop(myexent, snap="out")

mapbiomas_2012_2020 <- c(mapbiomas_2012,mapbiomas_2013, mapbiomas_2014,
                         mapbiomas_2015,mapbiomas_2016, mapbiomas_2017, 
                         mapbiomas_2018, mapbiomas_2019, mapbiomas_2020)

#Reclassify
# reclassify 
class_nomes <- read_excel("data/mapbiomas_AP_utm_rio/mapbiomas_6_legend.xlsx")
class_antropic <- class_nomes %>% 
  filter(type_class == "antropic") %>% pull(aid)
mapbiomas_2012_2020 <- classify(mapbiomas_2012_2020, cbind(0, NA))
reclass_m <- as.matrix(data.frame(human = class_antropic, new_value = 18))
mapbiomas_2012_2020 <- classify(mapbiomas_2012_2020, reclass_m)

mapbiomas_2012_2020_ag <- aggregate(mapbiomas_2012_2020, fact=5, fun="modal")

# colours and labels
vals12a20 <- unique(values(mapbiomas_2012_2020_ag))
class_vals <- unique(vals12a20[is.finite(vals12a20)])
class_color <- class_nomes %>% 
  dplyr::filter(aid %in% class_vals) %>% dplyr::pull(hexadecimal_code)
class_color <- paste("#", class_color, sep="")
names(class_color) <-  class_nomes %>% filter(aid %in% class_vals) %>% pull(aid)
#label
my_label <- class_nomes %>% 
  dplyr::filter(aid %in% class_vals) %>% dplyr::pull(classe_descricao)
my_label <- ifelse(my_label=="Agricultura", "Antropico", my_label)
names(my_label) <- class_nomes %>% filter(aid %in% class_vals) %>% pull(aid)

# animation
tm_ani <- tm_shape(mapbiomas_2012_2020_ag) + 
  tm_raster(style = "cat", 
            palette = class_color, legend.show =FALSE)  +
  tm_facets(ncol=1, nrow=1) 
saveRDS(mapbiomas_2012_2020_ag,"data/mapbiomas_2012_2020_ag.RDS")
saveRDS(class_color,"data/class_color.RDS")
