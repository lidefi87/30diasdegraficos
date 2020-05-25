# Dia 8 de 30 dias de graficos - Grafico de contorno
# Grafico basado en datos de CTD 

# Bibliotecas ---------------------------------------------------------------
library(tidyverse)

# Datos -------------------------------------------------------------------
#Accediendo a datos de CTD
CTD <- openxlsx::read.xlsx("../Datos/CTD_B1D.xlsx")
#Extrayendo datos del upcast (regreso del CTD desde fondo marino)
CTD <- CTD[which(CTD$Depth == max(CTD$Depth)):nrow(CTD),]

#Creando un modelo para obtener predicciones de salinidad basados en temperatura y profundidad
data.loess <- loess(Salinity ~ Depth * Temperature, data = CTD)

#Creando un dataframe con todas las combinaciones de temperatura y profundidad que se incrementa 
#por 0.1 unidades 
data.fit <- expand.grid(Temperature = seq(min(CTD$Temperature), max(CTD$Temperature), 0.1), 
                         Depth = seq(min(CTD$Depth), max(CTD$Depth), 0.5))

#Crear predicciones para cada combinacion de temperatura y profundidad basados en modelo loess
SalTempDep <- predict(data.loess, newdata = data.fit) %>% as.data.frame() %>% 
  #Preparando datos para uso en grafico
  rownames_to_column("Temperature") %>% as_tibble() %>% 
  pivot_longer(-Temperature, names_to = "Depth", names_prefix = "Depth=", values_to = "Salinity") %>% 
  mutate(Temperature = as.numeric(gsub("Temperature=", "", Temperature)),
         Depth = as.numeric(Depth))

# Graficos ----------------------------------------------------------------
g <- SalTempDep %>% 
  ggplot(aes(x = Temperature, y = Depth, z = Salinity))+
  geom_raster(aes(fill = Salinity))+
  scale_fill_distiller(palette = "YlGnBu", name = "Salinidad \n (PSU)")+
  geom_contour(colour = "white")+
  #Eje Y invertido para que la superficie este en la parte superior del grafico
  scale_y_reverse(lim = c(max(SalTempDep$Depth), min(SalTempDep$Depth)))+
  theme_bw()+
  labs(x = "Temperatura (C)", y = "Profundidad (m)",
       title = "Variacion temperatura y salinidad con profundidad")

#Guardando el grafico
ggsave("Outputs/Grafico8Contornos.png", g, device = "png")
