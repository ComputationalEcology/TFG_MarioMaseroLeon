

eneroras <- terra::rast("/vsicurl/https://zenodo.org/record/7213637/files/m1_h12_ground.tif")
enero.route <- calc_shaded_route("Calle Castilla, Sevilla", "San Jacinto, Sevilla", eneroras)

library(leaflet)
leaflet(sf::st_as_sf(enero.route)) |>
  leaflet::addWMSTiles(baseUrl = "https://www.ign.es/wms-inspire/ign-base",
  layers = "IGNBaseTodo-nofondo") |>
  leaflet::addTiles(urlTemplate =
    "https://mapasdesombra.github.io/Sevilla-jul-ground/13/{z}/{x}/{y}.png",
  options = leaflet::tileOptions(minZoom = 15, maxZoom = 18, tms = TRUE, opacity = 0.4)) |>
  addPolylines(weight = 8, opacity = 0.8)

