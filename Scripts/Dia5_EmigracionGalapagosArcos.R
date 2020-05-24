# Dia 5 de 30 dias de graficos - Diagramas de arco
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
  rename(c("cantonOrigen" = "id_can", "residenciaActual" = "M24"))

#Calculando el numero de conexiones por cada ubicacion
emig <- c(GalCen$cantonOrigen, GalCen$residenciaActual) %>%
  as.tibble() %>%
  group_by(value) %>%
  summarize(n = n()) %>% 
  rename(c("name" = "value")) %>% 
  #Creando grupos para grafico, Grupo 1 representa a cantones de origen en Galapagos, grupo 2
  #representa a provincias dentro del Ecuador y grupo 3 representa a otros paises
  mutate(grp = case_when(grepl("GPS", name) ~ 1,
                         name %in% Paises$pais ~ 3,
                         TRUE ~ 2),
         name = factor(name)) %>% 
  #Ordenando por grupo y numero de conexiones
  arrange(grp, desc(n))


# Graficos ----------------------------------------------------------------
#Creando un objeto graph para producir diagrama de arco con ggraph
g <- graph_from_data_frame(GalCen, vertices = emig, directed = FALSE)

#Haciendo el grafico, la opcion linear en layout es utilizada para hacer diagramas de arco
p1 <- ggraph(g, layout = "linear") + 
  #Definiendo como los arcos (conexiones) van a ser mostradas. Mientras mas gruesa y oscura, mas
  #conexiones existen entre puntos
  geom_edge_arc(edge_colour = "black", edge_alpha = 0.2, edge_width = 0.3) +
  #Definiendo como los puntos van a ser mostrados. El tamano varia de acuerdo al numero de conexiones
  #y el color varia de acuerdo a los grupos
  geom_node_point(aes(size = n, color = as.factor(grp), fill = grp), alpha = 0.5) +
  #Escala utilizada para variar el tamano de los puntos
  scale_size_continuous(range = c(0.75, 6)) +
  #Escala de colores colourbrewer utilizada
  scale_color_brewer(palette = "Dark2") +
  #Anadiendo las etiquetas rotadas a un angulo de 90 grados y movidas por debajo de los puntos
  geom_node_text(aes(label = name), angle = 90, size = 3, nudge_y = -.5, hjust = 1) +
  theme_void() +
  #Sacando la leyenda del grafico
  theme(legend.position = "none",
        #Editando el margin de la imagen
        plot.margin = unit(c(0, 0, 0.4, 0), "cm"), 
        #Centrando el titulo del grafico
        plot.title = element_text(hjust = 0.5))+
  #Expandiendo el limite del area del grafico
  expand_limits(y = c(-4, .5))+
  #Agregando un titulo al grafico
  labs(title = "Emigracion desde cantones de Galapagos en el 2015")

#Guardando imagen
ggsave("Outputs/Grafico5Arco.png", p1, device = "png")
