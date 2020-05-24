# Dia 1 de 30 dias de graficos de barras/columnas
# Grafico basado en datos de Data World sobre inteligencia canina

# Bibliotecas ---------------------------------------------------------------
library(tidyverse)

# Datos -------------------------------------------------------------------
# Accesando datos sobre inteligencia canina desde Data World
DogInt <- read.csv("https://download.data.world/s/uke5bpgyb7nanjxc5egpho4egzxnjd") %>%
  as_tibble()%>%
  #Niveles de inteligencia ordenados en niveles como indica Data World
  mutate(Classification = factor(Classification,
                                 levels = rev(c("Brightest Dogs", "Excellent Working Dogs",
                                                "Above Average Working Dogs",
                                                "Average Working/Obedience Intelligence",
                                                "Fair Working/Obedience Intelligence",
                                                "Lowest Degree of Working/Obedience Intelligence")),
                                 ordered = T))


# Graficos ----------------------------------------------------------------
# Inteligencia y peso
Graf1 <- DogInt %>%
  #Histograma con datos divididos cada 10 libras
  ggplot(aes(avgWeight))+geom_histogram(aes(fill = Classification), binwidth = 10)+
  #Cambiando titulos de ejes
  labs(x = "Average weight (lbs)", y = "Number of breeds")+
  #Haciendo el fondo blanco y negro
  theme_bw()+
  #Cambiando la ubicacion de la leyenda hacia la parte superior del grafico
  theme(legend.position = "top")+
  #Dividiendo la leyenda en dos columnas y poniendo el titulo en la parte superior de la leyenda 
  guides(fill = guide_legend(ncol = 2, title.position = "top"))
Graf1

ggsave("Outputs/Grafico1Barra.png", Graf1, device = "png", dpi = 300)
