# Dia 23 de 30 dias de graficos - Graficos Sunburst
# Grafico basado en especies de peces de las familias Lutjanidae y Carcharinidae encontradas en
# manglares de Galapagos durante muestreos en el 2014

# Bibliotecas -------------------------------------------------------------
library(plotly)
library(readr)
library(tidyverse)


# Datos -------------------------------------------------------------------
#Accediendo a base de datos de peces encontrados en manglares de Galapagos 
Peces <- read_csv("https://raw.githubusercontent.com/lidefi87/MangroveProject_CDF/master/Data/FishDB.csv") %>% 
  #Seleccionando columnas de interes
  select(ValidName, Family, Genus) %>% 
  #Seleccionando familias de interes
  filter(Family == "Carcharhinidae" | Family == "Lutjanidae") %>% 
  distinct() %>% 
  #Ignorando a filas con datos a nivel de familia o genero
  filter(!grepl("* sp$", ValidName)) %>% 
  #Quitando el caracter especial encontrado en Caranx sexfasciatus
  mutate(ValidName = case_when(grepl("*sexfasciatus$", ValidName) ~ "Caranx sexfasciatus",
                               TRUE ~ ValidName))

#Creando base de datos para crear grafico sunburst
df <- rbind(Peces %>% select(Family) %>% 
             mutate(N = 1) %>% 
             group_by(Family) %>% 
  mutate(ids = Family,
         labels = Family, 
         parents = "", 
         values = sum(N)) %>% 
    ungroup() %>% 
    select(-c(Family, N)),
  Peces %>% select(Family, Genus) %>% 
    mutate(N = 1) %>% 
    group_by(Family, Genus) %>% 
    mutate(ids = paste(Family, Genus, sep = " - "),
           labels = Genus,
           parents = Family,
           values = sum(N)) %>% 
    ungroup() %>% 
    select(-c(Family,Genus, N)),
  Peces %>% mutate(N = 1) %>% 
    group_by(Genus, ValidName) %>% 
    mutate(ids = ValidName,
           labels = ValidName,
           parents = Genus,
           values = sum(N)) %>%
    ungroup() %>% 
    select(ids, labels, parents, values)) %>% 
  distinct()


# Graficos ----------------------------------------------------------------
#Opcion branchvalues total resulta en un grafico que forma un circulo completo
g <- plot_ly(labels = df$labels, parents = df$parents, values = df$values, type = 'sunburst',
               branchvalues = "total")

#Guardando grafico interactivo
#Guardando el grafico interactivo
htmlwidgets::saveWidget(g, file = paste0(getwd(),"/Outputs/Grafico23Sunburst.html"))
#Guardando grafico interactivo como png
webshot::webshot("Outputs/Grafico23Sunburst.html", "Outputs/Grafico23Sunburst.png", delay = 20)
