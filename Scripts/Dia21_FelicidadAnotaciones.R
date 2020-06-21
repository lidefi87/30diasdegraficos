# Dia 21 de 30 dias de graficos con anotaciones
# Grafico basado en datos del World Happiness Report 2019 disponibles a traves de Datos de Miercoles

# Bibliotecas -------------------------------------------------------------
library(readr)
library(tidyverse)
library(ggpubr)
library(ggrepel)

# Datos -------------------------------------------------------------------
#Accediendo datos del World Happines Report 2019 desde el github de Datos de Miercoles
felicidad <- read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-08-07/felicidad.csv")
#Accediendo datos sobre regiones del mundo
region <- openxlsx::read.xlsx("../Datos/PaisesRegionesMundo.xlsx")

#Extrayendo datos para todos los países del 2018
Fel2018 <- felicidad %>% filter(anio == 2018) %>% 
  #Removiendo tildes
  mutate(pais = stringi::stri_trans_general(pais, "Latin-ASCII")) %>% 
  #Uniendo resultados del filtro con datos sobre regiones del mundo para producir grafico
  left_join(region %>% select(País, Continente), by = c("pais" = "País")) %>% 
  #Limpiando nombres
  janitor::clean_names()

# Graficos ----------------------------------------------------------------
#Graficando puntaje de felicidad anual por pais y guardando en variable
g <- Fel2018 %>% 
  drop_na(log_pib, escalera_vida, expectativa_vida) %>% 
  ggplot(aes(x = log_pib, y = escalera_vida, colour = continente, size = expectativa_vida))+
  geom_point(alpha = 0.5)+
  #Utilizando paleta de color de ColourBrewer para datos cualitativos (paises)
  scale_colour_brewer(palette = "Set1", name = "Continente")+
  scale_size(range = c(.1, 10), name = "Expectativa de vida")+
  scale_x_continuous(expand = expansion(mult = c(0.1, 0.2)))+
  scale_y_continuous(expand = expansion(mult = 0.1))+
  geom_text_repel(aes(label = ifelse(continente == "América" & 
                                       expectativa_vida > mean(Fel2018$expectativa_vida, na.rm = T),
                                     pais, "")),
                  nudge_x = 1.25,
                  direction = "both",
                  segment.size = 0.5,
                  size = 3)+
  theme_bw()+
  labs(x = "Log Producto Interno Bruto", y = "Puntaje de felicidad", 
       title = "Felicidad y expectativa de vida vs PIB en 2018")+
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Outputs/Grafico21Anotaciones.png", g, device = "png", dpi = 300)

