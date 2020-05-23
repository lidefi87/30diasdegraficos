
# Bibliotecas ---------------------------------------------------------------
library(tidyverse)

# Datos -------------------------------------------------------------------
# Accesando datos sobre inteligencia canina desde Data World
DogInt <- read.csv("https://download.data.world/s/uke5bpgyb7nanjxc5egpho4egzxnjd") %>% 
  as_tibble()%>% 
  mutate(Classification = factor(Classification, 
                                 levels = c("Brightest Dogs", "Excellent Working Dogs", 
                                            "Above Average Working Dogs", 
                                            "Average Working/Obedience Intelligence", 
                                            "Fair Working/Obedience Intelligence",
                                            "Lowest Degree of Working/Obedience Intelligence"), 
                                 ordered = T))

# Graficos ----------------------------------------------------------------
# Inteligencia y peso en perros
DogInt %>%
  ggplot(aes(avgWeight))+geom_histogram(aes(fill = Classification), binwidth = )+
  labs(x = "Average weight (lbs)", y = "Number of breeds")+
  theme_bw()+
  theme(legend.position = "top")+
  guides(fill=guide_legend(ncol = 2, title.position = "top"))
  
