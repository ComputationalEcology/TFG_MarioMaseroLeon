library(CityShadeMapper)
library(terra)
library(lidR)
library(rgl)
library(mapview)

#Leer nube de puntos (Tiene que ser .laz/.las)
las = readLAS("Data/")
las_check(las)

#Para ver la zona de estudio en un mapa
plot(las, mapview = TRUE, map.type = "Esri.WorldImagery")

#Para ver en 3D la nube de puntos
plot(las, size = 3)

#Crear raster para la clasificación de los puntos (.laz/.las)
triana.cover <- rasterize_lidar_cover_class("Data/")
plot(triana.cover)

#Calcular alturas
heights <- calc_heights_from_lidar(las)

#Calcular mapa de sombras (pon las fechas y horas que quieras mofificandolo abajo)
triana.canopy <- make_shademap(heights, date = c("2023-09-29", "2023-09-30", "2023-10-01"), hour = 12:15)
triana.cover = crop(triana.cover, triana.canopy)
triana.ground <- make_shademap_ground(triana.canopy, triana.cover)

#Generar SpatRaster a partir de .tif (Una vez que generes lo anterior lo puedes guardar en .tif
#y cargarlo directamente las siguientes veces)
triana.ground <- rast("Data/")
hasValues(triana.ground)

#Representar mapa de sombra
plot_shademap(triana.ground$`2023-09-29.13`, legend = FALSE, animate = FALSE, smooth = TRUE)

plasma <- ggplot() +
  geom_spatraster(data = triana.ground$`2023-09-30.13`) +
  scale_fill_viridis_c(option = "plasma", na.value = "transparent") +
  theme_void()

ggsave("plasma.png", plot = plasma, width = 16, units = "cm")
#Medias sombra/dia
global(triana.ground, "mean", na.rm = TRUE)

#---------------------------------------------------------------------------
#Extraer sombras de los lugares dónde tomé las temperaturas
library(terra)
library(sp)


coords_google <- data.frame(lat = , lon = ) #Coords de google (en decimal) de los puntos dónde se tomaron las temperaturas
coordinates(coords_google) <- c("lon", "lat")
proj4string(coords_google) <- CRS("+proj=longlat +datum=WGS84")
coords_utm <- spTransform(coords_google, CRS("+proj=utm +zone=30 +datum=WGS84"))


buffer_size <- 3  # tamaño del buffer en metros
buffered_plzasalesiano <- buffer(coords_utm, width = buffer_size, lonlat = FALSE)

library(sf) 
# Crear una lista para almacenar los objetos sf
buffers_sf <- list()

# Convertir cada objeto SpatialPolygons a formato sf y almacenarlo en la lista
for (nombre_objeto in nombres_objetos) {
  obj_sp <- get(nombre_objeto)
  obj_sf <- st_as_sf(obj_sp)
  buffers_sf[[nombre_objeto]] <- obj_sf
}

#Coger la lista y transformarlo todo en un único polígono
buffers_sf <- do.call(rbind, buffers_sf)

mapview(buffers_sf)
buffers_sf <- st_transform(buffers_sf, crs = "EPSG:3042") 

#Extraer sombra media de las calles
sombra.sept <- terra::extract(triana.ground, buffers_sf,
                                fun = "median", na.rm = TRUE)
sombra.sept <- round(sombra.sept, 2)
sombra.sept <- select(sombra.sept, -c('2023-09-29.12', '2023-09-30.12', '2023-10-01.12'))
columnas_modif <- c('2023-09-29.15', '2023-09-29.13', "2023-09-29.14",
                    '2023-09-30.15', '2023-09-30.13', "2023-09-30.14",
                    '2023-10-01.15', '2023-10-01.13', "2023-10-01.14")
sombra.sept <- sombra.sept |> 
  mutate_at(columnas_modif, list(~ 100 - .))

sombra.sept <- bind_cols(buffers_sf, sombra.sept)
sombra.sept <- sombra.sept |> 
  pivot_longer(cols = c('2023-09-29.15', '2023-09-29.13', "2023-09-29.14",
                        '2023-09-30.15', '2023-09-30.13', "2023-09-30.14",
                        '2023-10-01.15', '2023-10-01.13', "2023-10-01.14"),
               names_to = "FECHA", 
               values_to = "SOMBRA")

sombra.sept <- sombra.sept |> 
  group_by(ID, geometry) |> 
  summarise(SOMBRA = mean(SOMBRA, na.rm = TRUE),
            across(everything(), first, .names = "{.col}"),
            .groups = 'drop') |> 
  select(-FECHA)

sombra.sept <- read_xlsx("Data/sombra.sept2.xlsx") #Para cargar los datos que obtuve yo (generados por el código anterior)

#Conseguir el nombre de las calles para cada buffer
sombra.sept <- st_transform(sombra.sept, crs = st_crs(calles))
sombra.sept <- bind_cols(sombra.sept, nombres_objetos)
sombra.sept <- sombra.sept |>  rename(CALLES = ...7)

#Representar buffers en el mapa. (El objeto calles está generado con script Vect_Calles)
ggplot() +
  geom_sf(data = sombra.sept, aes(fill = 'blue')) + 
  geom_sf(data = calles, color = alpha("black", 0.5)) + 
  theme_minimal()