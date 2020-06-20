# Dia 16 de 30 dias de graficos - Graficos de Waffle
# Grafico basado en datos del INEC - Censo de poblacion y vivienda Galapagos 2015
# URL: https://www.ecuadorencifras.gob.ec/censo-de-poblacion-y-vivienda-galapagos/

# Bibliotecas ---------------------------------------------------------------
library(tidyverse)
library(waffle)
library(RColorBrewer)

# Datos -------------------------------------------------------------------
PobGps <- read.csv("../Datos/CensoPoblacionGalapagos2015/Poblacion_CPVG15_AT.csv", sep = ";") %>% 
  as_tibble() %>% janitor::clean_names() %>% 
  #Removiendo tildes en la columna area
  mutate(area = stringi::stri_trans_general(area, "Latin-ASCII")) %>% 
  #Seleccionando columnas de interes: area, sexo, edad
  select(area, psexo, p01, p09) %>% 
  #Renombrando columnas
  rename("edad" = "p01", "sexo" = "psexo", "nivelEdu" = "p09") %>% 
  #Seleccionando a personas de 20 anios o mas
  filter(edad >= 20 & edad <= 30) %>% 
  select(-edad) %>%
  #Agrupando por categorias y edad para calcular numero de personas totales en cada grupo 
  mutate(nivelEdu = factor(nivelEdu)) %>% 
  group_by(area, sexo, nivelEdu) %>% 
  summarise(N = n())

# Graficos ----------------------------------------------------------------
#Preparando grafico
g <- PobGps %>% 
  ggplot(aes(fill = nivelEdu, values = N/10))+
  geom_waffle(color = "white", size = 0.1, n_rows = 10)+
  facet_grid(area~sexo)+
  scale_x_discrete(expand = c(0, 0))+
  scale_y_discrete(expand = c(0, 0))+
  labs(title = "Niveles de educación en habitantes de Galápagos entre \n20 y 30 años (2015)",
       fill = "Nivel de educación")+
  theme(legend.position = "bottom")+
  guides(fill = guide_legend(nrow = 3, title.position = "top"))

ggsave("Outputs/Grafico16Waffles.png", g, device = "png", dpi = 300)


