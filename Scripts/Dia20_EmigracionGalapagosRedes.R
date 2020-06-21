# Dia 20 de 30 dias de graficos - Grafico de Redes
# Grafico basado en datos del INEC - Censo de poblacion y vivienda Galapagos 2015
# URL: https://www.ecuadorencifras.gob.ec/censo-de-poblacion-y-vivienda-galapagos/

# Bibliotecas ---------------------------------------------------------------
library(tidyverse)
library(ggraph)
library(igraph)

# Datos -------------------------------------------------------------------
#Accediendo datos sobre paises del mundo
Paises <- openxlsx::read.xlsx("../Datos/PaisesRegionesMundo.xlsx") %>% 
  #haciendo los nombres mas faciles para referencia
  janitor::clean_names()

#Subiendo datos de censo poblacional de Galapagos del 2015
GalCen <- read_csv("../Datos/CensoPoblacionGalapagos2015/Emigracion_CPVG15_AT.csv") %>% 
  #Seleccionando el canton de Galapagos donde se originan los migrantes (id_can) y el pais/provincia
  #donde los emigrantes residian al momento del censo
  select(id_can, M24) %>% 
  #Aumentando "GPS" a los sitios de origen de los migrantes
  mutate(id_can = paste0(id_can, " GPS"),
         #Cambiando los nombres de los sitios de residencia nuevo para que solo la primera letra sea
         #mayuscula
         M24 = str_to_title(M24),
         #Cambiando Republica de Corea a Corea del Sur para crear grupos para grafico
         M24 = case_when(grepl("Corea", M24) ~ "Corea del Sur",
                         TRUE ~ M24)) %>% 
  #Cambiando el nombre de las columnas
  rename(c("cantonOrigen" = "id_can", "residenciaActual" = "M24")) %>% 
  #Agrupando por canton de origen y contando las conexiones
  group_by(cantonOrigen) %>% 
  count(residenciaActual) %>% 
  pivot_wider(names_from = residenciaActual, values_from = n) %>% 
  mutate_if(is.numeric, replace_na, replace = 0) %>% 
  column_to_rownames("cantonOrigen") %>% 
  as.matrix()

#Creando una red que sera graficada
network <- graph_from_incidence_matrix(GalCen)

# Graficos ----------------------------------------------------------------
#Aumentando los margenes del grafico
par(mar = c(0,0,0,0))
#Preparando para guardar grafico
png("Outputs/Grafico20Redes.png", width = 800, height = 800)
#Creando el grafico de redes
plot(network,
     vertex.label.cex = 0.75)
#Incluyendo un titulo al grafico
title(main = "Emigracion desde Galapagos en el 2015")
dev.off()
