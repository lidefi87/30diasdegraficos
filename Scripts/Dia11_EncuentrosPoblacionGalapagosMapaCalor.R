# Dia 11 de 30 dias de graficos - Mapas de calor
# Grafico basado en datos del INEC - Censo de poblacion y vivienda Galapagos 2015
# URL: https://www.ecuadorencifras.gob.ec/censo-de-poblacion-y-vivienda-galapagos/

# Bibliotecas ---------------------------------------------------------------
library(tidyverse)

# Datos -------------------------------------------------------------------
PobGps <- read.csv("../Datos/CensoPoblacionGalapagos2015/Poblacion_CPVG15_AT.csv", sep = ";") %>% 
  as_tibble() %>% janitor::clean_names() %>% 
  #Removiendo tildes en la columna area
  mutate(area = stringi::stri_trans_general(area, "Latin-ASCII")) %>% 
  #Seleccionando columnas de interes: area, sexo, edad
  select(area, p01, p09, gedad) %>% 
  #Renombrando columnas
  rename("edad" = "p01", "nivelEdu" = "p09", "grupoEdad" = "gedad") %>% 
  #Seleccionando a personas de 20 anios o mas
  filter(edad >= 20) %>% 
  select(-edad) %>%
  #Agrupando por area para calcular numero total de habitantes por area
  group_by(area) %>% 
  mutate(N_area = n()) %>%
  #Uniendo columnas de area y nivel de educacion para formar categorias para el grafico
  unite("Categorias", area:nivelEdu, sep = "-", remove = T) %>% 
  #Agrupando por categorias y edad para calcular numero de personas totales en cada grupo 
  group_by(Categorias, grupoEdad) %>% 
  summarise(N_area = mean(N_area),
            N_CatEd = n()) %>% 
  #Calcular porcentajes por area
  mutate(Prop = N_CatEd/N_area) %>% 
  #Desagrupar para completar categorias
  ungroup() %>%
  complete(Categorias, nesting(grupoEdad)) 

# Graficos ----------------------------------------------------------------
g <- PobGps %>% ggplot(aes(x = grupoEdad, y = Categorias, fill = Prop)) + 
  geom_tile()+theme(axis.text.x = element_text(angle = 90))+
  scale_fill_distiller(palette = "YlOrRd")+
  labs(x = "", y = "", title = "% de habitantes de areas urbana y rural con diversos \n
       niveles de educacion en Galapagos 2015")+
  theme(plot.title = element_text(hjust = 0.75),
        plot.margin = unit(c(0, 0, 0.4, 0), "cm"))+
  expand_limits(y = c(-4, .5))

#Guardando grafico
ggsave("Outputs/Grafico11MapaCalor.png", g, device = "png", dpi = 300)
