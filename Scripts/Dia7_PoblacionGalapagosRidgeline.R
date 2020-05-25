# Dia 7 de 30 dias de graficos - Grafico ridgeline
# Grafico basado en datos del INEC - Censo de poblacion y vivienda Galapagos 2015
# URL: https://www.ecuadorencifras.gob.ec/censo-de-poblacion-y-vivienda-galapagos/

# Bibliotecas ---------------------------------------------------------------
library(tidyverse)
library(ggridges)

# Datos -------------------------------------------------------------------
#Accediendo a datos de poblacion de Galapagos del 2015
PobGps <- read.csv("../Datos/CensoPoblacionGalapagos2015/Poblacion_CPVG15_AT.csv", sep = ";") %>% 
  as_tibble() %>% janitor::clean_names() %>% 
  #Removiendo tildes en la columna area
  mutate(area = stringi::stri_trans_general(area, "Latin-ASCII")) %>% 
  #Seleccionando columnas de interes: area, sexo, edad
  select(area, psexo, p01) %>% 
  #Renombrando columnas
  rename("sexo" = "psexo", "edad" = "p01") %>% 
  #Uniendo columnas de area y sexo para formar categorias para el grafico
  unite("Categorias", area:sexo, sep = "-", remove = T)


# Graficos ----------------------------------------------------------------
#Edades por categorias con el color variando de acuerdo a la edad
g <- PobGps %>% ggplot(aes(x = edad, y = Categorias, fill = ..x..))+
  #Creando el grafico ridgeline, scale define la altura maxima de los picos y rel_min_height
  #indica los datos que seran removidos de las colas
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01)+
  #Usando paleta de colores especifica
  scale_fill_distiller(palette = "YlOrRd", direction = 1)+
  labs(title = "Distribucion de edades por genero en \n areas urbanas y rurales de Galapagos 2015",
       x = "Edades")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

#Guardando grafico
ggsave("Outputs/Grafico7Ridgeline.png", device = "png")
