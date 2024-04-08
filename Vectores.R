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

mask <- st_read("Data/Distritos_de_Sevilla.shp")
mask <- st_set_crs(mask, st_crs(trianamapa))
mask <- st_geometry(mask)
trianamapa <- st_filter(trianamapa, mask)

#Mostrar mapa
par(mar = rep(0, 4))
plot(st_geometry(trianamapa))

#Extraer sombra media de las calles
trianavect <- st_as_sf(trianamapa)
mapview(trianavect)

calles <- trianavect |> 
  group_by(name) |> 
  summarise()

sombra.calles <- extract(triana.ground, vect(calles), fun = "mean", na.rm = TRUE)
sombra.calles <- round(sombra.calles, 2)
sombra.calles <- bind_cols(calles, sombra.calles)

#Representación sombra a nv de suelo
library(RColorBrewer)

plot(triana.ground, y = 1, axes = FALSE,
     col = RColorBrewer::brewer.pal(9, "YlOrRd"),
     main = "Sombra por Calles 29-09-2023 13:00")
plot(vect(trianavect), add = TRUE)

#Representación sombra por calles
library(RColorBrewer)

plot(sombra.calles[4],
     main = "Sombra por Calles 29-09-2023 13:00")

