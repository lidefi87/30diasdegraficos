# Dia 4 de 30 dias de graficos - Graficos con facetas
# Grafico basado en datos de Global Shark Attack File (GSAF)

# Bibliotecas ---------------------------------------------------------------
library(tidyverse)

# Datos -------------------------------------------------------------------
# Subiendo datos sobre encuentros con tiburones del GSAF
TibuEncu <- readxl::read_excel("../Datos/SharkEncountersData.xlsx", range = "A1:P6522") %>% 
  #Cambiando nombres de columnas para que sean mas claros
  janitor::clean_names() %>% 
  #Seleccionando columnas de interes
  select(year, type, country, fatal_y_n) %>% 
  #Eliminando observaciones sin pais o clasificacion sobre fatalidad
  drop_na(country, fatal_y_n) %>% 
  #Filtrando casos con informacion sobre fatalidad
  filter(fatal_y_n == "Y" | fatal_y_n == "N") %>% 
  mutate(fatal_y_n = case_when(fatal_y_n == "Y" ~ "Fatal",
                               fatal_y_n == "N" ~ "Non fatal")) %>% 
  #Filtrando casos provocados o no provocados (columna type)
  filter(type == "Provoked" | type == "Unprovoked") %>% 
  #Cambiando columna a numerica para seleccionar datos anuales
  mutate(country = factor(country)) %>%
  #Escogiendo datos entre 2010 y 2020
  filter(year %in% seq(2010, 2020)) %>%
  #Escogiendo paises con 10 o mas observaciones
  right_join(., TibuEncu %>% group_by(country) %>% 
               summarise(N = n()) %>% filter(N >= 10), by = "country") %>% 
  #Produciendo un sumario de encuentros
  group_by(year, country, fatal_y_n, type) %>% 
  summarise(N = n())


# Graficos ----------------------------------------------------------------
g1 <- TibuEncu %>% ggplot(aes(x = year, y = N, group = country))+
  facet_grid(type~fatal_y_n, scales = "free_y")+
  geom_point(aes(color = country))+
  labs(y = "")+
  geom_line(aes(color = country))+
  scale_color_brewer(palette = "Paired", name = "Countries")+
  theme(legend.position = "top", axis.text.x = element_text(angle = 90))+
  guides(colour = guide_legend(ncol = 3, title.position = "top"))

ggsave("Outputs/Grafico4conFacetas.png", g1, device = "png", dpi = 300)
