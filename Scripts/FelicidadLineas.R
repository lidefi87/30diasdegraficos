# Dia 2 de 30 dias de graficos de lineas
# Grafico basado en datos del World Happiness Report 2019 disponibles a traves de Datos de Miercoles

# Bibliotecas -------------------------------------------------------------
library(readr)
library(tidyverse)
library(RColorBrewer)
library(ggpubr)

# Datos -------------------------------------------------------------------
#Accediendo datos del World Happines Report 2019 desde el github de Datos de Miercoles
felicidad <- read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-08-07/felicidad.csv")

#Paises sudamericanos de interes para este grafico
PaisesSudAm <- c("Colombia", "Venezuela", "Guyana", "Surinam", "Ecuador", "Perú", "Brasil", "Bolivia",
                 "Paraguay", "Chile", "Uruguay", "Argentina")

#Extrayendo datos de paises sudamericanos solamente
FelSudAm <- felicidad %>% filter(pais %in% PaisesSudAm) %>% 
  #Excluyendo paises para los que solo hay un anio de datos disponibles
  group_by(pais) %>% summarise(N = n()) %>% filter(N > 1) %>% 
  select(pais) %>% left_join(., felicidad, by = "pais")

# Graficos ----------------------------------------------------------------
#Graficando puntaje de felicidad anual por pais y guardando en variable
FelLines <- FelSudAm %>% ggplot(aes(x = anio))+
  geom_point(aes(y = escalera_vida, colour = pais))+
  geom_line(aes(y = escalera_vida, colour = pais))+
  #Utilizando paleta de color de ColourBrewer para datos cualitativos (paises)
  scale_colour_brewer(palette = "Paired", name = "País")+
  theme_bw()+
  labs(x = "Año", y = "Puntaje de felicidad")

#Graficando expectativa de vida anual por pais y guardando en variable
ExpVidaLines <- FelSudAm %>% ggplot(aes(x = anio))+
  geom_point(aes(y = expectativa_vida, colour = pais))+
  geom_line(aes(y = expectativa_vida, colour = pais))+
  #Utilizando paleta de color de ColourBrewer para datos cualitativos (paises)
  scale_colour_brewer(palette = "Paired", name = "País")+
  theme_bw()+
  labs(x = "Año", y = "Expectativa de vida")

#Uniendo los dos graficos en uno utilizando dos filas y compartiendo leyenda
FelExpVida <- ggarrange(FelLines, ExpVidaLines, nrow = 2, common.legend = T)

#Guardando grafico
ggsave("Outputs/Grafico2Lineas.png", FelExpVida, device = "png", dpi = 300)
  
  

