#------------------------------------------------------------------------------
#JULIO
library(terra)

julioras <- terra::rast("https://zenodo.org/records/7212277/files/Sevilla_shademap_july_ground_cog.tif")
julioras_mañana <- subset(julioras, 4:7)
julioras_medio <- subset(julioras, 8:11)
julioras_tarde <- subset(julioras, 12:15)

julioras_mañana <- terra::crop(julioras_mañana, "Data/Distritos.geojson")
julioras_medio <- terra::crop(julioras_medio, "Data/Distritos.geojson")
julioras_tarde <- terra::crop(julioras_tarde, "Data/Distritos.geojson")

#Extraccion de sombras
sombra.julio.man <- terra::extract(julioras_man, terra::vect(green_index),
                               fun = "mean", na.rm = TRUE)
sombra.julio.man <- round(sombra.julio.man, 2)
sombra.julio.man <- bind_cols(green_index, sombra.julio.man)
sombra.julio.man <- sombra.julio.man |> 
  select(-green_index_green_area, -green_index) |> 
  rename(green_index = green_index_tree)

sombra.julio.medio <- terra::extract(julioras_medio, terra::vect(green_index),
                                   fun = "mean", na.rm = TRUE)
sombra.julio.medio <- round(sombra.julio.medio, 2)
sombra.julio.medio <- bind_cols(green_index, sombra.julio.medio)
sombra.julio.medio <- sombra.julio.medio |> 
  select(-green_index_green_area, -green_index) |> 
  rename(green_index = green_index_tree)

sombra.julio.tarde <- terra::extract(julioras_tarde, terra::vect(green_index),
                                   fun = "mean", na.rm = TRUE)
sombra.julio.tarde <- round(sombra.julio.tarde, 2)
sombra.julio.tarde <- bind_cols(green_index, sombra.julio.tarde)
sombra.julio.tarde <- sombra.julio.tarde |> 
  select(-green_index_green_area, -green_index) |> 
  rename(green_index = green_index_tree)
#-----------------------------------------------------------------------------
#ENERO
eneroras <- terra::rast("https://zenodo.org/records/7212277/files/Sevilla_shademap_january_ground_cog.tif")
eneroras_man <- subset(eneroras, 5:8)
eneroras_tarde <- subset(eneroras, 7:10)

eneroras_man <- terra::crop(eneroras_man, "Data/Distritos.geojson")
eneroras_tarde <- terra::crop(eneroras_tarde, "Data/Distritos.geojson")

sombra.enero.man <- terra::extract(eneroras_man, terra::vect(green_index),
                                   fun = "mean", na.rm = TRUE)
sombra.enero.man <- round(sombra.enero.man, 2)
sombra.enero.man <- bind_cols(green_index, sombra.enero.man)

sombra.enero.tarde <- terra::extract(eneroras_tarde, terra::vect(green_index),
                                     fun = "mean", na.rm = TRUE)
sombra.enero.tarde <- round(sombra.enero.tarde, 2)
sombra.enero.tarde <- bind_cols(green_index, sombra.enero.tarde)

#Extraccion sombras (buffers disponibles en la carpeta Data en formato .geojson)
sombra.enero <- terra::extract(eneroras_man, buffers_sf,
                               fun = "median", na.rm = TRUE)
sombra.enero <- round(sombra.enero, 2)
columnas_modif <- c('2022-01-15.13', '2022-01-15.14', '2022-01-15.15', '2022-01-15.16')
sombra.enero <- sombra.enero |> 
  mutate_at(columnas_modif, list(~ 100 - .))

sombra.enero <- bind_cols(buffers_sf, sombra.enero)
sombra.enero <- sombra.enero |> 
  pivot_longer(cols = c('2022-01-15.13', '2022-01-15.14', '2022-01-15.15', '2022-01-15.16'),
               names_to = "FECHA", 
               values_to = "SOMBRA")

sombra.enero <- sombra.enero |> 
  group_by(ID, geometry) |> 
  summarise(SOMBRA = mean(SOMBRA, na.rm = TRUE),
            across(everything(), first, .names = "{.col}"),
            .groups = 'drop') |> 
  select(-FECHA)
#Conseguir el nombre de las calles para cada buffer
sombra.enero <- st_transform(sombra.enero, crs = st_crs(calles))
sombra.enero <- bind_cols(sombra.enero, nombres_objetos)
sombra.enero <- sombra.enero |>  rename(CALLES = ...4)

#-----------------------------------------------------------------------------
#Rutas por la sombra (en este caso lo voy a hacer con julio)
shaderas <- terra::rast("/vsicurl/https://zenodo.org/record/7213637/files/m7_h12_ground.tif")
shade.route1 <- calc_shaded_route("Calle Betis 68, 41010", "Calle San Jorge, Sevilla", shaderas)
shade.route2 <- calc_shaded_route("Calle Giralda, 41010", "Calle San Jorge, Sevilla", shaderas)
shade.route3 <- calc_shaded_route("Calle Trabajo, 41010", "Calle San Jorge, Sevilla", shaderas)
shade.route4 <- calc_shaded_route("Calle Aracena, 41010", "Calle San Jorge, Sevilla", shaderas)
shade.route5 <- calc_shaded_route("Calle Salado, 41010", "Calle San Jorge, Sevilla", shaderas)
shade.route6 <- calc_shaded_route("Calle Rafael Belmonte 10, 41010", "Calle San Jorge, Sevilla", shaderas)
#-----------------------------------------------------------------------
library(leaflet)
shade.route <- leaflet() |>
  leaflet::addWMSTiles(baseUrl = "https://www.ign.es/wms-inspire/ign-base",
                       layers = "IGNBaseTodo-nofondo") |>
  leaflet::addTiles(urlTemplate =
                      "https://mapasdesombra.github.io/Sevilla-jul-ground/12/{z}/{x}/{y}.png",
                    options = leaflet::tileOptions(minZoom = 15, maxZoom = 18, tms = TRUE, opacity = 0.4)) |>
  addPolylines(data = sf::st_as_sf(shade.route1), weight = 8, opacity = 0.7, color = "red") |> 
  addPolylines(data = sf::st_as_sf(shade.route2), weight = 8, opacity = 0.7, color = "blue") |> 
  addPolylines(data = sf::st_as_sf(shade.route3), weight = 8, opacity = 0.7, color = "darkgreen") |> 
  addPolylines(data = sf::st_as_sf(shade.route4), weight = 8, opacity = 0.7, color = "purple") |> 
  addPolylines(data = sf::st_as_sf(shade.route5), weight = 8, opacity = 0.7, color = "yellow") |> 
  addPolylines(data = sf::st_as_sf(shade.route6), weight = 8, opacity = 0.7, color = "hotpink")



