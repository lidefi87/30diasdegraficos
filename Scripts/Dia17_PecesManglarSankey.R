# Dia 17 de 30 dias de graficos - Diagrama de Sankey
# Grafico basado en datos de censos de peces en areas de manglar en Galapagos en el 2014
# URL: https://www.ecuadorencifras.gob.ec/censo-de-poblacion-y-vivienda-galapagos

# Bibliotecas -------------------------------------------------------------
library(networkD3)
library(dplyr)

# Datos -------------------------------------------------------------------
#Accediendo a datos de censos en areas de manglar. Manteniendo solo informacion de una region y un
#metodo. Sitios muestreados en las filas y especies muestreados en columnas.
data2 <- read.csv("../Datos/FishAbundanceAllPts.csv") %>% 
  filter(Method == "BRUV" & Bioregion == "Western") %>% 
  column_to_rownames(var = "X") %>% 
  select(-c(X.1, Bioregion, Method))

#Creando base de datos con informacion sobre las conexiones entre sitios muestreados y especies
#de peces. Solo se mantienen vinculos si existe al menos una observacion
links2 <- data2 %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "source") %>% 
  pivot_longer(-source, names_to = "target", values_to = "value") %>% 
  filter(value > 0)

#Creando una lista con los nombres de los nodos - puntos de inicio y fin entre conexiones
nodes2 <- data.frame(name = c(as.character(links2$source), as.character(links2$target))) %>% 
  distinct()

#Agregando informacion sobre conexiones a base inicial (links2)
#Se resta uno a las conexiones porque cero indica que es el inicio al crear el grafico con la 
#funcion sanketNetwork
links2$IDsource <- match(links2$source, nodes2$name)-1 
links2$IDtarget <- match(links2$target, nodes2$name)-1

# Graficos ----------------------------------------------------------------
#Creando grafico interactivo con la biblioteca networkD3
p <- sankeyNetwork(Links = links2, Nodes = nodes2,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name",
              sinksRight = F)
p

#Guardando el grafico interactivo
htmlwidgets::saveWidget(p, file = paste0(getwd(),"/Outputs/Grafico17Sankey.html"))
