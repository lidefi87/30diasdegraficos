# Dia 19 de 30 dias de graficos - Graficos de flujo
# Grafico basado en datos de Global Shark Attack File (GSAF)

# Bibliotecas ---------------------------------------------------------------
library(tidyverse)
library(streamgraph)

# Datos -------------------------------------------------------------------
#Lista de paises latinoamericanos
LatAm <- c("Brazil", "Belize", "Chile", "Colombia", "Costa Rica", "Cuba", "Dominican Republic",
           "Ecuador", "Honduras", "Mexico", "Panama", "Puerto Rico", "Uruguay", "Venezuela")

# Subiendo datos sobre encuentros con tiburones del GSAF
TibuEncu <- readxl::read_excel("../Datos/SharkEncountersData.xlsx", range = "A1:P6522") %>% 
  #Cambiando nombres de columnas para que sean mas claros
  janitor::clean_names() %>% 
  #Seleccionando columnas de interes
  select(year, country) %>% 
  #Corrigiendo el pais de Columbia a Colombia
  mutate(country = case_when(country == "COLUMBIA" ~ "Colombia",
                             TRUE ~ country)) %>% 
  #Eliminando observaciones sin pais o clasificacion sobre fatalidad
  drop_na(country) %>% 
  #Escogiendo datos entre 2000 y 2020
  filter(year %in% seq(2000, 2020)) %>%
  #Cambiando los nombres de paises de mayusculas a solo la primera en mayuscula
  mutate(country = str_to_title(country)) %>% 
  #Manteniendo solo paises latinoamericanos
  filter(country %in% LatAm) %>% 
  #Cambiando columna de pais a factor y la de anio a numerica
  mutate(country = factor(country),
         year = as.numeric(year)) %>%
  #Produciendo un sumario de encuentros por pais y anio
  group_by(year, country) %>% 
  summarise(N = as.numeric(n())) %>% 
  #Completando observaciones por pais por anio
  complete(country, nesting(year)) %>% 
  #Cambiando NAs (de completado arriba) a ceros para el grafico
  mutate(N = replace_na(N, 0))

# Graficos ----------------------------------------------------------------
#Grafico de numero de encuentros por anio
g <- TibuEncu %>% streamgraph(key = "country", value = "N", date = "year", 
                         height="300px", width="1000px") %>% 
  sg_axis_x(tick_interval = 1, tick_units = "year") %>% 
  sg_legend(show = T, label = "Paises")

#Guardando el grafico interactivo
htmlwidgets::saveWidget(g, file = paste0(getwd(),"/Outputs/Grafico19Flujo.html"))
