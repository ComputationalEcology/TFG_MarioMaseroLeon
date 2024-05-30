library(readxl)
library(tidyverse)
library(patchwork)

#Importar datos
sombra.calles <- read.csv("Data/sombra.calles.csv", stringsAsFactors = FALSE)

#-----------------------------------------------------------------------------
#MANEJO DATOS DE JULIO
library(sf)
st_write(trianamapa, "Data/trianamapa.csv")

nombresTriana <- read.csv("Data/trianamapa.csv")
nombresTriana <- nombresTriana |> 
  select(osm_id:name)
sombra.julio.man <- merge(sombra.julio.man, nombresTriana)
rm(nombresTriana)
sombra.julio.man <- sombra.julio.man |> 
  pivot_longer(cols = c(`2022-07-15.11`, `2022-07-15.12`, `2022-07-15.13`, `2022-07-15.14`), 
               names_to = "FECHA", 
               values_to = "SOMBRA")

sombra.julio.man.calles <- sombra.julio.man |> 
  rename(CALLES = name) |> 
  group_by(CALLES)|> 
  mutate(CALLES = replace(CALLES, CALLES == "", NA)) |> 
  filter(CALLES == "Ronda de Triana" | CALLES == "Ronda de los Tejares" |
         CALLES == "Calle Pureza" | CALLES == "Calle Betis" |
         CALLES == "Calle San Jacinto" | CALLES == "Calle Pagés del Corro" |
         CALLES == "Calle Alfarería" | CALLES == "Calle Castilla" |
         CALLES == "Calle Salado" | CALLES == "Calle Manuel Arellano" |
         CALLES == "Avenida Santa Cecilia" | CALLES == "Avenida de Coria" |
         CALLES == "Calle Giralda" | CALLES == "Calle Trabajo" |
         CALLES == "Calle Evangelista" | CALLES == "Calle Esperanza de Triana" |
         CALLES == "Calle Rodrigo de Triana" | CALLES == "Paseo Nuestra Señora de la O" |
         CALLES == "Calle Clara de Jesús Montero" | CALLES == "Calle Niculoso Pisano") |> 
  pivot_longer(cols = c(`2022-07-15.11`, `2022-07-15.12`, `2022-07-15.13`, `2022-07-15.14`), 
               names_to = "FECHA", 
               values_to = "SOMBRA") |> 
  select(-ID)
sombra.julio.man.calles <- na.omit(sombra.julio.man.calles)

nombresTriana <- read.csv("Data/trianamapa.csv")
nombresTriana <- nombresTriana |> 
  select(osm_id:name)
sombra.julio.medio <- merge(sombra.julio.medio, nombresTriana)
sombra.julio.medio <- sombra.julio.medio |> 
  pivot_longer(cols = c(`2022-07-15.15`, `2022-07-15.16`, `2022-07-15.17`, `2022-07-15.18`), 
               names_to = "FECHA", 
               values_to = "SOMBRA")
rm(nombresTriana)
  
sombra.julio.medio.calles <- sombra.julio.medio |> 
  rename(CALLES = name) |> 
  group_by(CALLES)|> 
  mutate(CALLES = replace(CALLES, CALLES == "", NA)) |> 
  filter(CALLES == "Ronda de Triana" | CALLES == "Ronda de los Tejares" |
           CALLES == "Calle Pureza" | CALLES == "Calle Betis" |
           CALLES == "Calle San Jacinto" | CALLES == "Calle Pagés del Corro" |
           CALLES == "Calle Alfarería" | CALLES == "Calle Castilla" |
           CALLES == "Calle Salado" | CALLES == "Calle Manuel Arellano" |
           CALLES == "Avenida Santa Cecilia" | CALLES == "Avenida de Coria" |
           CALLES == "Calle Giralda" | CALLES == "Calle Trabajo" |
           CALLES == "Calle Evangelista" | CALLES == "Calle Esperanza de Triana" |
           CALLES == "Calle Rodrigo de Triana" | CALLES == "Paseo Nuestra Señora de la O" |
           CALLES == "Calle Clara de Jesús Montero" | CALLES == "Calle Niculoso Pisano") |> 
  select(-ID) |> 
  pivot_longer(cols = c(`2022-07-15.15`, `2022-07-15.16`, `2022-07-15.17`, `2022-07-15.18`), 
               names_to = "FECHA", 
               values_to = "SOMBRA") 
sombra.julio.medio.calles <- na.omit(sombra.julio.medio.calles)

nombresTriana <- read.csv("Data/trianamapa.csv")
nombresTriana <- nombresTriana |> 
  select(osm_id:name)
sombra.julio.tarde <- merge(sombra.julio.tarde, nombresTriana)
rm(nombresTriana)

sombra.julio.tarde.calles <- sombra.julio.tarde |> 
  rename(CALLES = name) |> 
  group_by(CALLES)|> 
  mutate(CALLES = replace(CALLES, CALLES == "", NA)) |> 
  filter(CALLES == "Ronda de Triana" | CALLES == "Ronda de los Tejares" |
           CALLES == "Calle Pureza" | CALLES == "Calle Betis" |
           CALLES == "Calle San Jacinto" | CALLES == "Calle Pagés del Corro" |
           CALLES == "Calle Alfarería" | CALLES == "Calle Castilla" |
           CALLES == "Calle Salado" | CALLES == "Calle Manuel Arellano" |
           CALLES == "Avenida Santa Cecilia" | CALLES == "Avenida de Coria" |
           CALLES == "Calle Giralda" | CALLES == "Calle Trabajo" |
           CALLES == "Calle Evangelista" | CALLES == "Calle Esperanza de Triana" |
           CALLES == "Calle Rodrigo de Triana" | CALLES == "Paseo Nuestra Señora de la O" |
           CALLES == "Calle Clara de Jesús Montero" | CALLES == "Calle Niculoso Pisano") |> 
  select(-ID) |> 
  pivot_longer(cols = c(`2022-07-15.19`, `2022-07-15.20`, `2022-07-15.21`), 
               names_to = "FECHA", 
               values_to = "SOMBRA") 
sombra.julio.tarde.calles <- na.omit(sombra.julio.tarde.calles)

#Establecer nv sombra (100 - x)
columnas_modif <- c('SOMBRA')
sombra.julio.man <- sombra.julio.man |> 
  mutate_at(columnas_modif, list(~ 100 - .))
sombra.julio.man.calles <- sombra.julio.man.calles |> 
  mutate_at(columnas_modif, list(~ 100 - .))

sombra.julio.medio <- sombra.julio.medio |> 
  mutate_at(columnas_modif, list(~ 100 - .))
sombra.julio.medio.calles <- sombra.julio.medio.calles |> 
  mutate_at(columnas_modif, list(~ 100 - .))

sombra.julio.tarde <- sombra.julio.tarde |> 
  mutate_at(columnas_modif, list(~ 100 - .))
sombra.julio.tarde.calles <- sombra.julio.tarde.calles |> 
  mutate_at(columnas_modif, list(~ 100 - .))

rm(columnas_modif)

#MANEJO DATOS ENERO
nombresTriana <- read.csv("Data/trianamapa.csv")
nombresTriana <- nombresTriana |> 
  select(osm_id:name)
sombra.enero.man <- merge(sombra.enero.man, nombresTriana)
rm(nombresTriana)

sombra.enero.man.calles <- sombra.enero.man |> 
  rename(CALLES = name) |> 
  group_by(CALLES)|> 
  mutate(CALLES = replace(CALLES, CALLES == "", NA)) |> 
  filter(CALLES == "Ronda de Triana" | CALLES == "Ronda de los Tejares" |
           CALLES == "Calle Pureza" | CALLES == "Calle Betis" |
           CALLES == "Calle San Jacinto" | CALLES == "Calle Pagés del Corro" |
           CALLES == "Calle Alfarería" | CALLES == "Calle Castilla" |
           CALLES == "Calle Salado" | CALLES == "Calle Manuel Arellano" |
           CALLES == "Avenida Santa Cecilia" | CALLES == "Avenida de Coria" |
           CALLES == "Calle Giralda" | CALLES == "Calle Trabajo" |
           CALLES == "Calle Evangelista" | CALLES == "Calle Esperanza de Triana" |
           CALLES == "Calle Rodrigo de Triana" | CALLES == "Paseo Nuestra Señora de la O" |
           CALLES == "Calle Clara de Jesús Montero" | CALLES == "Calle Niculoso Pisano") |> 
  pivot_longer(cols = c(`2022-07-15.11`, `2022-07-15.12`, `2022-07-15.13`, `2022-07-15.14`), 
               names_to = "FECHA", 
               values_to = "SOMBRA") |> 
  select(-ID)
sombra.enero.man.calles <- na.omit(sombra.enero.man.calles)

nombresTriana <- read.csv("Data/trianamapa.csv")
nombresTriana <- nombresTriana |> 
  select(osm_id:name)
sombra.enero.medio <- merge(sombra.enero.medio, nombresTriana)
rm(nombresTriana)

sombra.enero.medio.calles <- sombra.enero.medio |> 
  rename(CALLES = name) |> 
  group_by(CALLES)|> 
  mutate(CALLES = replace(CALLES, CALLES == "", NA)) |> 
  filter(CALLES == "Ronda de Triana" | CALLES == "Ronda de los Tejares" |
           CALLES == "Calle Pureza" | CALLES == "Calle Betis" |
           CALLES == "Calle San Jacinto" | CALLES == "Calle Pagés del Corro" |
           CALLES == "Calle Alfarería" | CALLES == "Calle Castilla" |
           CALLES == "Calle Salado" | CALLES == "Calle Manuel Arellano" |
           CALLES == "Avenida Santa Cecilia" | CALLES == "Avenida de Coria" |
           CALLES == "Calle Giralda" | CALLES == "Calle Trabajo" |
           CALLES == "Calle Evangelista" | CALLES == "Calle Esperanza de Triana" |
           CALLES == "Calle Rodrigo de Triana" | CALLES == "Paseo Nuestra Señora de la O" |
           CALLES == "Calle Clara de Jesús Montero" | CALLES == "Calle Niculoso Pisano") |> 
  pivot_longer(cols = c(`2022-07-15.15`, `2022-07-15.16`, `2022-07-15.17`, `2022-07-15.18`), 
               names_to = "FECHA", 
               values_to = "SOMBRA") |>
  select(-ID)
sombra.enero.medio.calles <- na.omit(sombra.enero.medio.calles)

nombresTriana <- read.csv("Data/trianamapa.csv")
nombresTriana <- nombresTriana |> 
  select(osm_id:name)
sombra.enero.tarde <- merge(sombra.enero.tarde, nombresTriana)
rm(nombresTriana)

sombra.enero.tarde.calles <- sombra.enero.tarde |> 
  rename(CALLES = name) |> 
  group_by(CALLES)|> 
  mutate(CALLES = replace(CALLES, CALLES == "", NA)) |> 
  filter(CALLES == "Ronda de Triana" | CALLES == "Ronda de los Tejares" |
           CALLES == "Calle Pureza" | CALLES == "Calle Betis" |
           CALLES == "Calle San Jacinto" | CALLES == "Calle Pagés del Corro" |
           CALLES == "Calle Alfarería" | CALLES == "Calle Castilla" |
           CALLES == "Calle Salado" | CALLES == "Calle Manuel Arellano" |
           CALLES == "Avenida Santa Cecilia" | CALLES == "Avenida de Coria" |
           CALLES == "Calle Giralda" | CALLES == "Calle Trabajo" |
           CALLES == "Calle Evangelista" | CALLES == "Calle Esperanza de Triana" |
           CALLES == "Calle Rodrigo de Triana" | CALLES == "Paseo Nuestra Señora de la O" |
           CALLES == "Calle Clara de Jesús Montero" | CALLES == "Calle Niculoso Pisano") |> 
  pivot_longer(cols = c(`2022-07-15.19`, `2022-07-15.20`, `2022-07-15.21`), 
               names_to = "FECHA", 
               values_to = "SOMBRA") |>
  select(-ID)
sombra.enero.tarde.calles <- na.omit(sombra.enero.tarde.calles)

#Establecer nv sombra (100 - x) (para los .calles) (hacer previamente el pivot_longer)
columnas_modif <- c('SOMBRA')
sombra.enero.man <- sombra.enero.man |> 
  mutate_at(columnas_modif, list(~ 100 - .))
sombra.enero.man.calles <- sombra.enero.man.calles |> 
  mutate_at(columnas_modif, list(~ 100 - .))

sombra.enero.medio <- sombra.enero.medio |> 
  mutate_at(columnas_modif, list(~ 100 - .))
sombra.enero.medio.calles <- sombra.enero.medio.calles |> 
  mutate_at(columnas_modif, list(~ 100 - .))

sombra.enero.tarde <- sombra.enero.tarde |> 
  mutate_at(columnas_modif, list(~ 100 - .))
sombra.enero.tarde.calles <- sombra.enero.tarde.calles |> 
  mutate_at(columnas_modif, list(~ 100 - .))

rm(columnas_modif)

#MANEJO DATOS SEPTIEMBRE
sombra.sept <- read_xlsx("Data/sombraseptiembre.xlsx")
sombra.sept <- sombra.sept |> 
  group_by(ID) |> 
  summarise(SOMBRA = mean(SOMBRA, na.rm = TRUE),
            across(everything(), first, .names = "{.col}"),
            .groups = 'drop') |> 
  select(-FECHA, -ORIENTACIÓN, - geometry) |> 
  mutate(DIFTEMP = TSOL - TSOMBRA)


#-----------------------------------------------------------------------------
#Representaciones
library(osmdata)
library(tidyterra)
library(mapSpain)
library(scales)
library(viridis)
library(viridisLite)
library(patchwork)

basetriana <- opq('Triana, Sevilla') %>%
  add_osm_feature(key = 'name', value = "Triana") %>%
  osmdata_sf()
basetriana <- basetriana$osm_multipolygons$geometry
basetriana <- st_transform(basetriana, crs = st_crs(triana_bbox))
basetriana <- st_crop(basetriana, triana_bbox)
basetriana <- esp_getTiles(basetriana, "IGNBase.Gris",
                           bbox_expand = 0.1, zoommin = 1)

plot_3 <- ggplot()+
  geom_spatraster_rgb(data = basetriana, alpha = 0.9) +
  geom_sf(data = filter(sombra.julio.medio, FECHA == "2022-07-15.15"), aes(color = SOMBRA),
          lwd = 1) +
  scale_color_viridis_c(option = "viridis", na.value = "transparent", direction = -1) +
  labs(x = "", y = "") +
  theme_minimal()

plot_medio <- ggplot()+
  geom_spatraster_rgb(data = basetriana) +
  geom_sf(data = analisis_medio, aes(color = `SOMBRA`),
          lwd = 1.25) +
  scale_color_viridis_b("Nivel de sombra", alpha = 0.75, direction = -1, values = viridis_pal(8))
  
plot_tarde <- ggplot()+
  geom_spatraster_rgb(data = basetriana) +
  geom_sf(data = sombra.julio.tarde.calles, aes(color = `SOMBRA`),
          lwd = 1.25) +
  facet_wrap(~ FECHA) +
  scale_color_viridis_b("Nivel de sombra", alpha = 0.75, direction = -1, values = viridis_pal(8))

plot_julio <- wrap_plots(plot_1, plot_2, plot_3)
plot_julio
rm(plot_man)
rm(plot_medio)
rm(plot_tarde)

