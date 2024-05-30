library(readxl)
library(tidyverse)
library(patchwork)

#Importar datos de las temperaturas que fueron tomadas
verano_medidas <- read_excel("Data/verano_medidas.xlsx")
View(verano_medidas)

invierno_medidas <- read_excel("Data/invierno_medidas.xlsx")
View(invierno_medidas)

sombra.calles <- read.csv("Data/sombra.calles.csv", stringsAsFactors = FALSE)

#Diferencias
dif_verano <- verano_medidas |> 
  mutate(DifTemp = SOL - SOMBRA)

dif_invierno <- invierno_medidas |> 
  mutate(DifTemp = SOL - SOMBRA)

#Transformación tablas
verano_medidas <- verano_medidas |> 
  pivot_longer(cols = c(SOL, SOMBRA), 
               names_to = "SOL-SOMBRA", 
               values_to = "TEMPERATURA")

invierno_medidas <- invierno_medidas |> 
  pivot_longer(cols = c(SOL, SOMBRA), 
               names_to = "SOL-SOMBRA", 
               values_to = "TEMPERATURA")

#Representación gráficas
solvssombra <- ggplot(
  data = verano_medidas,
  mapping = aes(x = TEMPERATURA, y = CALLES)) +
  geom_point(aes(color = `SOL-SOMBRA`, shape = `SOL-SOMBRA`), size = 3) +
  geom_line(aes(group = CALLES), color = "grey", alpha = 1) +
  facet_grid(verano_medidas$FECHA, scales = "free") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(vjust = 0.5, hjust = 1),
  )
  
ggsave("Data/Sol vs Sombra Verano.png", plot = solvssombra, width = 13, height = 24.7, units = "cm")

#-------------------------------------------------
library(ggplot2)
library(ggforce)
library(ggdist)
library(gghalves)


plot_veranotemp <- ggplot(
  data = verano_medidas,
  mapping = aes(x = `SOL-SOMBRA`, y = TEMPERATURA, fill = `SOL-SOMBRA`)) +
  xlab("") +
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3) +
  geom_boxplot(colour = "black", alpha = 0.75, width = .1, outlier.shape = NA) +
  gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .5, 
                            mapping = aes(color = `SOL-SOMBRA`)) +
  ylim(0, 60) +
  labs(subtitle = "Verano") +
  theme_minimal() +
  theme(plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none") 

plot_inviernotemp <- ggplot(
  data = invierno_medidas,
  mapping = aes(x = `SOL-SOMBRA`, y = TEMPERATURA, fill = `SOL-SOMBRA`)) +
  xlab("") +
  ylab("") +
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3) +
  geom_boxplot(colour = "black", alpha = 0.75, width = .1, outlier.shape = NA) +
  gghalves::geom_half_point(side = "l", range_scale = .4, alpha = .5, 
                            mapping = aes(color = `SOL-SOMBRA`)) +
  ylim(0, 60) +
  labs(subtitle ="Invierno") +
  theme_minimal() +
  theme(plot.subtitle = element_text(hjust = 0.5))

plot_mediatemp <- wrap_plots(plot_veranotemp, plot_inviernotemp) +
  plot_annotation(title = "Media temperaturas en el sol y en la sombra",
                  theme = theme(plot.title = element_text(face = "bold",
                                                          hjust = 0.5)))

ggsave("raincloud.png", plot = plot_mediatemp, width = 23.7, height = 8, units = "cm")

#-----------------------------------------------------------------------------
#Scatterplot sol/sombra con ecuación de la recta
library(tidyverse)
library(patchwork)

verano_medidas <- verano_medidas %>%
  mutate(ESTACION = "Verano")

invierno_medidas <- invierno_medidas %>%
  mutate(ESTACION = "Invierno")

medidas_combined <- bind_rows(verano_medidas, invierno_medidas)

solvssombra <- ggplot(medidas_combined, aes(x = SOL, y = SOMBRA, color = ESTACION)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0) +
  theme_light() +
  labs(x = "Temperaturas al sol") +
  xlim(0, 60) +
  ylim(0, 50) +
  theme(plot.subtitle = element_text(hjust = 0.5), legend.text = element_text(size = 10),
        legend.position = "right") +
  scale_color_manual(values = c("Verano" = "red", "Invierno" = "cyan3"))

ggsave("sol-sombra.png", plot = solvssombra, width = 23.7, units = "cm")

#(El objeto sombra.enero se genera a través del Script RasterEnJul)
sombra.sept <- read_xlsx("Data/sombra.sept2.xlsx")
sombra.sept <- sombra.sept |> 
  mutate(MES = "Septiembre")

sombra.enero <- read_xlsx("Data/medidasenero.xlsx")
sombra.enero <- sombra.enero |> 
  mutate(MES = "Enero")

sombra_combined <- bind_rows(sombra.sept, sombra.enero)

ggplot(sombra_combined, aes(x = SOMBRA, y = TSOMBRA, color = MES)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  geom_abline(slope = 1, intercept = 0) +
  theme_light() +
  xlim(0, 100) +
  ylim(10, 50) +
  theme(plot.subtitle = element_text(hjust = 0.5),
        legend.position = "right")

#Los lmer son generados en el script AnalisisDatos
augment_data <- broom.mixed::augment(lmer_sept, data = sombra.sept)
augment_data2 <- broom.mixed::augment(lmer_enero, data = sombra.enero)
augment_data$Season <- "Septiembre"
augment_data2$Season <- "Enero"
combined_data <- rbind(augment_data, augment_data2)

solsombra <- ggplot(combined_data, aes(x = SOMBRA, y = TEMPERATURA)) + 
  geom_point(aes(color = `SOL-SOMBRA`), alpha = 0.5) +
  geom_line(aes(y = .fitted, color = Season), lwd = 1.2) +  
  geom_abline(slope = 1, intercept = 0) +
  theme_minimal() +
  theme(plot.subtitle = element_text(hjust = 0.5),
        legend.position = "right",
        text = element_text(size = 14))  +
  labs(x = "Nivel de Sombra") +
  scale_color_manual(values = c("Septiembre" = "red", "Enero" = "cyan3",
                                "TSOL" = "orange", "TSOMBRA" = "blue"))
ggsave("sol-sombra.png", plot = , width = 23.7, units = "cm")
