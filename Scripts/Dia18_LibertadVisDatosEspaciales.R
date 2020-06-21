# Dia 3 de 30 dias de graficos de puntos
# Grafico basado en datos del World Happiness Report 2019 disponibles a traves de Datos de Miercoles

# Bibliotecas -------------------------------------------------------------
library(readr)
library(tidyverse)
library(sf)

# Datos -------------------------------------------------------------------
#Accediendo datos del World Happines Report 2019 desde el github de Datos de Miercoles
WHR <- read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-08-07/felicidad.csv")

#Cargando base de datos con codigos ISO de paises 
RegPais <- read_csv("../Datos/UNSD_PaisesRegionesMundo.csv") %>% 
  janitor::clean_names()

#Creando base de datos con indice de libertad a nivel mundial del anio 2018 
Libertad <- WHR %>% 
  filter(anio == 2018) %>% 
  dplyr::select(pais, libertad) %>% 
  mutate(pais = case_when(pais == "Bolivia" ~ "Bolivia (Estado Plurinacional de)",
                          pais == "Venezuela" ~ "Venezuela (República Bolivariana de)",
                          pais == "Estados Unidos" ~ "Estados Unidos de América",
                          TRUE ~ pais)) %>% 
  #Agregando columna de codigos ISO
  left_join(RegPais, by = c("pais"="country_or_area"))
  
#Cargando mapa del mundo
WorldMap <- sf::st_read("../Datos/WorldCountries/ne_50m_admin_0_countries.shp") %>% 
  #Agregando indice de libertad a los datos de la capa
  left_join(Libertad, by = c("ADM0_A3"="iso_alpha3_code")) 

# Graficos ----------------------------------------------------------------
#Ploteando mapa
g <- ggplot(WorldMap)+
  #color del pais basado en indice de libertad
  geom_sf(aes(fill = libertad))+
  #cambiando limites del mapa para mostrar las Americas solamente
  coord_sf(xlim = c(-180, -10), ylim = c(-60, 90), expand = FALSE)+
  scale_fill_distiller(palette = "GnBu", direction = -1, name = "Indice de libertad")+
  labs(title = "Libertad para tomar decisiones de vida en las Americas (2018)")+
  theme(legend.position = "bottom", legend.direction = "horizontal",
        plot.title = element_text(hjust = 0.5))+
  guides(fill = guide_colorbar(title.position = "top"))

ggsave("Outputs/Grafico18DatosEspaciales.png", g, device = "png", dpi = 300)
