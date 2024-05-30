library(car)
library(lme4)
library(lmerTest)

#Lmer de las temperaturas tomadas para verano e invierno al sol y a la sombra

verano_medidas <- na.omit(verano_medidas)
Temp <- lmer(verano_medidas$TEMPERATURA ~ verano_medidas$`SOL-SOMBRA` + (1|ID), data = verano_medidas, REML=TRUE)
summary(Temp)
Anova(Temp, type="II")

Temp2 <- lmer(invierno_medidas$TEMPERATURA ~ invierno_medidas$`SOL-SOMBRA` + (1|ID), data = invierno_medidas, REML=TRUE)
summary(Temp2)
Anova(Temp2, type="II")

#Lm temperaturas medias verano e invierno

lm_verano <- lm(SOMBRA ~ SOL, data = verano_medidas)
summary(lm_verano)

lm_invierno <- lm(SOMBRA ~ SOL, data = invierno_medidas)
summary(lm_invierno)

#Lm de la orientación/cobertura vegetal/ anchura calle con respecto a la sombra. Objetos sombra.julio.man/man.calles en el script Tablas
library(visreg)

analisis_medio <- sombra.julio.man.calles |> 
  group_by(osm_id, geometry) |> 
  summarise(SOMBRA = mean(SOMBRA, na.rm = TRUE),
            across(everything(), first, .names = "{.col}"),
            .groups = 'drop') |> 
  select(-FECHA)

analisis_total <- sombra.julio.man |> 
  group_by(osm_id, geometry) |> 
  summarise(SOMBRA = mean(SOMBRA, na.rm = TRUE),
            across(everything(), first, .names = "{.col}"),
            .groups = 'drop') |> 
  select(-FECHA, -ID)

analisis_medio <- read_xlsx("Data/analisis_medio.xlsx")
analisis <- select(sombra.julio.medio.calles, geometry, osm_id)
analisis <- analisis |> distinct()
analisis_medio <- merge(analisis_medio, analisis, by = "osm_id")
analisis_medio <- st_as_sf(analisis_medio)

lm_medio <- lm(SOMBRA ~ green_index + ORIENTACION + ANCHURA, data = analisis_medio)
summary(lm_medio)

raincloud <- analisis_medio |> 
  ggplot(aes(y = SOMBRA, x = ORIENTACION, fill = ORIENTACION)) +
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3) +
  geom_boxplot(colour = "black", alpha = 0.75, width = .1, outlier.shape = NA) +
  gghalves::geom_half_point(aes(color = ORIENTACION), side = "l", range_scale = .4, alpha = .5) +
  theme_minimal() +
  xlab("") +
  ylab("Nivel de sombra") +
  labs(fill = "ORIENTACIÓN", color = "ORIENTACIÓN") +
  theme(plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none",
        text = element_text(size = 14)) 

ggsave("raincloud.png", plot = raincloud, width = 23.7, height = 12, units = "cm")

greeindex <- ggplot(analisis_medio, aes(x = green_index, y = SOMBRA)) +
  geom_point(alpha = 0.5, color = "green4") +
  geom_smooth(method = "lm", color = "green4") +
  geom_abline(slope = 1, intercept = 0) +
  theme_light() +
  theme(plot.subtitle = element_text(hjust = 0.5),
        legend.position = "right") +
  theme(plot.subtitle = element_text(hjust = 0.5),
        legend.position = "right",
        text = element_text(size = 14))

#Representar orientación
library(ggforce)
library(ggdist)
library(gghalves)

sombra.julio.medio.calles |> 
  ggplot(
    mapping = aes(x = ORIENTACION, y = SOMBRA, fill = ORIENTACION)) +
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = c(0.5, 1)) +
  gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .5, 
                            mapping = aes(color = ORIENTACION)) +
  geom_boxplot(colour = "black", alpha = 0.75, width = .1, outlier.shape = NA) +
  labs(subtitle = "JULIO MAÑANA") +
  theme(plot.subtitle = element_text(hjust = 0.5))

#Lmer de los valores de sombra extraidos a través de los buffers

sombra.sept <- read_xlsx("Data/sombra.sept2.xlsx")
sombra.sept <- sombra.sept |> 
  pivot_longer(cols = c(TSOL, TSOMBRA), 
             names_to = "SOL-SOMBRA", 
             values_to = "TEMPERATURA")
lmer_sept <- lmer(TEMPERATURA ~ SOMBRA + (1|ID), data = sombra.sept2, REML=TRUE)
summary(lmer_sept)


sombra.enero <- read_xlsx("Data/medidasenero.xlsx")
sombra.enero <- sombra.enero |> 
  pivot_longer(cols = c(TSOL, TSOMBRA), 
               names_to = "SOL-SOMBRA", 
               values_to = "TEMPERATURA")
lmer_enero <- lmer(TEMPERATURA ~ SOMBRA + (1|ID), data = sombra.enero, REML=TRUE)
summary(lmer_enero)
