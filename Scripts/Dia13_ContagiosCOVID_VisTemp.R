# Dia 13 de 30 dias de graficos - Visualizar datos temporales
# Grafico basado en casos confirmados de COVID-19 del Our world in Data
# https://ourworldindata.org/grapher/daily-covid-cases-3-day-average

# Bibliotecas -------------------------------------------------------------
library(tidyverse)
library(unikn)
library(ggpubr)

# Datos -------------------------------------------------------------------
#Base de datos con nombres y regiones de la UNSD en ingles
PaisesRegiones <- read_csv("../Datos/UNSD_CountriesRegionsWorld.csv")

#Accediendo datos de casos confirmados de COVID-19 del Our world in Data
COVID <- read_csv("../Datos/daily-covid-cases-3-day-average.csv") %>% 
  rename("casosConf" = "Daily new confirmed cases of COVID-19 (rolling 3-day average, right-aligned)") %>% 
  #Dando el formato de fecha a la columna fecha
  mutate(Date = parse_datetime(Date, format = "%b %d, %Y")) %>% 
  #Uniendo bases de datos para obtener la region del mundo donde se encuentra cada pais
  left_join(PaisesRegiones %>% select(`ISO-alpha3 Code`, `Region Name`, 
                                      `Intermediate Region Name`),
            by = c("Code"="ISO-alpha3 Code")) %>% 
  janitor::clean_names() %>% 
  #Incluyendo informacion de regiones de paises no incluidos en la base de datos de la UNSD
  mutate(region_name = case_when(entity == "Kosovo" ~ "Europe",
                                 entity == "Taiwan" ~ "Asia",
                                 TRUE ~ region_name)) %>% 
  #Excluyendo datos globales
  drop_na(region_name)

# Graficos ----------------------------------------------------------------
#Produciendo figura apilada de casos confirmados de COVID-19 a nivel mundial por region 
g1 <- COVID %>% 
  #Obteniendo totales diarios por region
  group_by(date, region_name) %>% 
  summarise(casosTotales = sum(casos_conf, na.rm = T)) %>% 
  ggplot(aes(x = date, y = casosTotales))+ 
  geom_area(aes(fill = region_name), size = 0.25, colour = "grey", alpha = 0.8)+
  scale_fill_brewer(palette = "Dark2", name = "Regiones del mundo")+
  theme_bw()+
  labs(x = "", y = "")+
  theme(legend.position = "top",
        plot.margin = unit(c(4.5, 4.5, 1, 1), "pt"))+
  guides(fill = guide_legend(title.position = "top"))

#Produciendo figura de lineas para casos en las Americas
g2 <- COVID %>% filter(intermediate_region_name == "South America") %>% 
  #Removiendo numero de casos negativos (explicacion para estos numeros no encontrada en fuente)
  filter(casos_conf>=0) %>% 
  ggplot(aes(x = date, y = casos_conf))+
  geom_line(aes(color = entity), size = 0.75)+
  theme_bw()+
  scale_color_manual(values = usecol(pal = pal_unikn_pair), name = "Países de Sur América")+
  theme(legend.position = "top",
        plot.margin = unit(c(4.5, 4.5, 1, 1), "pt"))+
  guides(colour = guide_legend(title.position = "top", ncol = 5))+
  labs(x = "", y = "")

#Uniendo dos graficos en uno
g <- ggarrange(g1, g2, nrow = 2, labels = c("A", "B"))
g <- annotate_figure(g, left = text_grob("Casos totales (media acumulada por 3 días)", 
                                         rot = 90, hjust = 0.5, size = 12), 
                     top = "Casos confirmados de COVID-19")

#Guardando graficos
ggsave("Outputs/Grafico13VisTemp.png", g, device = "png")
