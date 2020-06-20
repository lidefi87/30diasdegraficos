# Dia 16 de 30 dias de graficos - Graficos de Waffle
# Grafico basado en datos del INEC - Censo de poblacion y vivienda Galapagos 2015
# URL: https://www.ecuadorencifras.gob.ec/censo-de-poblacion-y-vivienda-galapagos/

# Bibliotecas ---------------------------------------------------------------
library(tidyverse)
library(waffle)

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
  filter(edad >= 20) %>% 
  select(-edad) %>%
  #Uniendo columnas de area y nivel de educacion para formar categorias para el grafico
  unite("Categorias", sexo:nivelEdu, sep = "-", remove = T) %>% 
  #Agrupando por categorias y edad para calcular numero de personas totales en cada grupo 
  group_by(area, Categorias) %>% 
  summarise(N = n()) %>% 
  #Desagrupar para completar categorias
  ungroup() %>%
  complete(Categorias, nesting(area)) 

# Graficos ----------------------------------------------------------------
#Preparando grafico
PobGps %>% treemap(., index = c("area", "Categorias"), vSize = "N",
                   fontsize.labels = c(15,12),
                   fontcolor.labels = c("black", "white"),
                   bg.labels = "transparent",
                   align.labels = list(c("center", "center"),
                                       c("left", "top")),
                   overlap.labels = 0.5,
                   border.col = c("black", "white"), 
                   palette = "Dark2",
                   title = "Niveles de educación en habitantes de 20 años o más en las Galápagos (2015)")