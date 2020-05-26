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
region <- openxlsx::read.xlsx("../Datos/PaisesRegionesMundo.xlsx")

#Extrayendo datos para todos los países del 2018
Fel2018 <- felicidad %>% filter(anio == 2018) %>% 
  #Removiendo tildes
  mutate(pais = stringi::stri_trans_general(pais, "Latin-ASCII")) %>% 
  #Uniendo resultados del filtro con datos sobre regiones del mundo para producir grafico
  left_join(region %>% select(País, Continente), by = c("pais" = "País")) %>% 
  #Limpiando nombres
  janitor::clean_names() %>% 
  #Seleccionando columnas de interes
  drop_na(pais, expectativa_vida) %>% 
  #Escogiendo America y Europa para comparar
  filter(continente == "Europa" | continente == "América") %>%
  #Transformar pais en factor y reordenar basado en la expectativa de vida
  mutate(pais = factor(pais)) %>% 
  mutate(pais = forcats::fct_reorder(pais, expectativa_vida))
  

# Graficos ----------------------------------------------------------------
g <- Fel2018 %>% ggplot(aes(y = expectativa_vida, x = pais))+
  #Cambiando el color de los segmentos y punto por continente para mejorar contraste
  geom_point(aes(color = continente))+
  geom_segment(aes(yend = expectativa_vida, y = 0, xend = pais, color = continente))+
  labs(y = "Expectativa de vida", x = "", 
       title = "Expectativa de vida en países de las Americas y Europa \n en el 2018")+
  #Diviendo datos entre continentes
  facet_wrap(.~continente, ncol = 2, scales = "free_y")+
  #Intercambiando ejes
  coord_flip()+
  theme_bw()+
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 1))
  
#Guardando grafico
ggsave("Outputs/Grafico12Paletas.png", g, device = "png", dpi = 300)
