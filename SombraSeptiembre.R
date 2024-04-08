library(CityShadeMapper)
library(terra)
library(lidR)
library(rgl)
library(mapview)

#Leer nube de puntos
las = readLAS("triana.laz")
las_check(las)

#Para ver la zona de estudio en un mapa
plot(las, mapview = TRUE, map.type = "Esri.WorldImagery")

#Para ver en 3D la nube de puntos
plot(las, size = 3)

#Crear raster para la clasificación de los puntos
triana.cover <- rasterize_lidar_cover_class("triana.laz")
plot(triana.cover)

#Calcular alturas
heights <- calc_heights_from_lidar(las)

#Calcular mapa de sombras
triana.canopy <- make_shademap(heights, date = c("2023-09-29", "2023-09-30", "2023-10-01"), hour = 13:15)
triana.cover = crop(triana.cover, triana.canopy)
triana.ground <- make_shademap_ground(triana.canopy, triana.cover)

#Generar SpatRaster a partir de .tif
triana.ground <- rast("test.tif")
hasValues(triana.ground)

#Representar mapa de sombra
plot_shademap(triana.ground, legend = FALSE, animate = TRUE, smooth = TRUE)

#Medias sombra/dia
global(triana.ground, "mean", na.rm = TRUE)

#Ruta con sombras (no funciona tengo que averiguar por qué)
shade.route <- calc_shaded_route("Calle San Jacinto, Sevilla", "Ronda de Triana, Sevilla", triana.ground)
