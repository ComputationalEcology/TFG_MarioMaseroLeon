library(greenR)

#Generar datos
verdetriana <- get_osm_data("Triana, Sevilla, Spain")
areaverde <- verdetriana$green_areas
visualize_green_spaces(areaverde)

#Recorte triana (a partir del .laz)
triana_bbox <- st_bbox(las)

green_index <- calculate_green_index(verdetriana, 25829, 100)
green_index <- st_as_sf(green_index)
green_index <- st_crop(green_index, triana_bbox)

#Visualizacion mapa interactivo
breaks <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
labels <- c("0-0,2", "0,21-0,4", "0,41-0,6", "0,61-0,8", "0,81-1")
green_index$tramos <- cut(green_index$green_index, breaks = breaks, labels = labels, right = FALSE)


basetriana <- opq('Triana, Sevilla') %>%
  add_osm_feature(key = 'name', value = "Triana") %>%
  osmdata_sf()
basetriana <- basetriana$osm_multipolygons$geometry
basetriana <- st_transform(basetriana, crs = st_crs(triana_bbox))
basetriana <- st_crop(basetriana, triana_bbox)
basetriana <- esp_getTiles(basetriana, "IGNBase.Gris",
                           bbox_expand = 0.1, zoommin = 1)

custom_palette <- c("0-0,2" = "#87070b",  
                    "0,21-0,4" = "#FF4000", 
                    "0,41-0,6" = "#FF8000",
                    "0,61-0,8" = "#80FF00", 
                    "0,81-1" = "#307a1b") 

ggplot() +
  geom_spatraster_rgb(data = basetriana, alpha = 0.85) +
  geom_sf(data = green_index, aes(color = tramos, fill = tramos), lwd = 1.05) +
  scale_color_manual(values = custom_palette) +
  scale_fill_manual(values = custom_palette) +
  theme_minimal()

#Porcentaje cobertura vegetal
porcentaje <- calculate_percentage(sombra.julio.man)

