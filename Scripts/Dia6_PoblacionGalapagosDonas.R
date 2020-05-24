# Dia 6 de 30 dias de graficos - Grafico de donut
# Grafico basado en datos del INEC - Censo de poblacion y vivienda Galapagos 2015
# URL: https://www.ecuadorencifras.gob.ec/censo-de-poblacion-y-vivienda-galapagos/

# Bibliotecas ---------------------------------------------------------------
library(tidyverse)
library(sunburstR)

# Datos -------------------------------------------------------------------
#Accediendo a datos de poblacion de Galapagos del 2015
PobGps <- read.csv("../Datos/CensoPoblacionGalapagos2015/Poblacion_CPVG15_AT.csv", sep = ";") %>% 
  as_tibble() %>% janitor::clean_names() %>% 
  #Removiendo tildes en la columna area
  mutate(area = stringi::stri_trans_general(area, "Latin-ASCII")) %>% 
  #Seleccionando columnas de interes: area, sexo, edad, nivel de educacion
  select(area, psexo, gedad, p09) %>% 
  #Renombrando columnas
  rename("sexo" = "psexo", "grupoEdad" = "gedad", "nivEdu" = "p09") %>% 
  #Uniendo todas las columnas para formar un camino describiendo el grafico de donas de varios niveles
  unite("path", area:nivEdu, sep = "-", remove = T) %>% 
  #Incluir una columna de unos (1) para contar observaciones
  mutate(index = 1)

#Creando grafico de donas interactivo de varios niveles
g <- sunburst(PobGps)

#Guardando grafico como html
htmltools::save_html(g, file = paste0(getwd(),"/Outputs/Grafico6Donas.html"))
