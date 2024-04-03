library(CityShadeMapper)
library(terra)
library(lidR)
library(rgl)

las = readLAS("Trianarecortada.laz")
#las_check(las)

#Para ver la zona de estudio en un mapa
#plot(las, mapview = TRUE, map.type = "Esri.WorldImagery")

#Para ver en 3D la nube de puntos
plot(las, size = 3)

#Crear raster para la clasificación de los puntos
triana.cover <- rasterize_lidar_cover_class("Trianarecortada.laz")
plot(triana.cover)

#Calcular alturas
heights <- calc_heights_from_lidar(las)

#Calcular mapa de sombras
triana.canopy <- make_shademap(heights, date = "2023-09-29", hour = 13:15)
triana.cover = crop(triana.cover, triana.canopy)
triana.ground <- make_shademap_ground(triana.canopy, triana.cover)

#Representar mapa de sombra
plot_shademap(triana.ground, legend = FALSE, animate = TRUE, smooth = TRUE)

#Ruta con sombras
shade.route <- calc_shaded_route("Calle San Jacinto, Sevilla", "Ronda de Triana, Sevilla", triana.ground)
