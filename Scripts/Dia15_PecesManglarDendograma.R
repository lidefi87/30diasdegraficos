# Dia 15 de 30 dias de graficos - Dendrogramas
# Grafico basado en datos obtenidos en muestreo de peces en areas de manglar en las Galapagos

# Bibliotecas ---------------------------------------------------------------
library(tidyverse)

# Datos -------------------------------------------------------------------
#Accediendo a datos de peces en los manglares de Galapagos
Fish <- read_csv("../Datos/FishAbundanceAllPts.csv") %>% 
  filter(Method == "BRUV" & Bioregion == "CSE") %>% 
  column_to_rownames("X1") %>% 
  select(-c(X120:Method)) %>% 
  as.matrix()

#Calculando distancia Bray-Curtis utilizando el paquete vegan
dist <- vegan::vegdist(Fish)

#Identificando grupos (clusters)
FishClust <- hclust(dist)

# Graficos ----------------------------------------------------------------
#Preparando para guardar grafico
png("Outputs/Grafico15Dendrograma.png", width = 800, height = 800)
#Produciendo dendrograma
FishDen <- as.dendrogram(FishClust)
#Graficando clusters
plot(FishDen)
title(main = "Dendograma de comunidades de peces de manglar de Galápagos")
dev.off()
