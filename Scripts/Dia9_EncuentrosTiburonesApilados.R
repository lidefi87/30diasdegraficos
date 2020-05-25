time <- as.numeric(rep(seq(1,7),each=7))  # x Axis
value <- runif(49, 10, 100)               # y Axis
group <- rep(LETTERS[1:7],times=7)        # group, one shape per group
data <- data.frame(time, value, group)

library(tidyverse)
ggplot(data, aes(x=time, y=value, fill=group)) + 
  geom_area()



# Dia 9 de 30 dias de graficos - Graficos de areas apiladas
# Grafico basado en datos de Global Shark Attack File (GSAF)

# Bibliotecas ---------------------------------------------------------------
library(tidyverse)

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
g <- TibuEncu %>% ggplot(aes(x = year, y = N))+
  #Datos apilados por pais, linea incluida entre pais para ayudar a diferenciar mejor entre paises
  geom_area(aes(fill = country), size = 0.25, colour = "black")+
  theme_bw()+
  labs(y = "Número de encuentros humanos-tiburones", x = "Año")+
  theme(legend.position = "top")+
  guides(fill = guide_legend(ncol = 4, title.position = "top",
                             title = "Países Latinoamericanos"))

#Guardando grafico
ggsave("Outputs/Grafico9Apilados.png", g, device = "png", dpi = 300)
