library(osmdata)
library(sf)
library(tidyverse)
library(ggspatial)

#Crear mapa calle Triana
trianamapa <- opq('Triana, Sevilla') %>%
  add_osm_feature(key = 'highway') %>%
  osmdata_sf()

#Transformar poligonos a lineas
trianamapa <- osm_poly2line(trianamapa)

#Escoger solo las lineas
trianamapa <- trianamapa$osm_lines

#Recortar mapa
triana_bbox <- st_bbox(las)
trianamapa <- st_transform(trianamapa, crs = st_crs(triana_bbox))
trianamapa <- st_crop(trianamapa, triana_bbox)

#Mostrar mapa
par(mar = rep(0, 4))
plot(st_geometry(trianamapa))

#Extraer sombra media de las calles
trianavect <- st_as_sf(trianamapa)
mapview(trianavect)
rm(trianamapa)

calles <- trianavect |> 
  group_by(name) |> 
  summarise()

sombra.calles <- terra::extract(triana.ground, vect(calles),
                         fun = "mean", na.rm = TRUE)
sombra.calles <- round(sombra.calles, 2)
sombra.calles <- bind_cols(calles, sombra.calles)
