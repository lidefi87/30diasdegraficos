# Dia 22 de 30 dias de graficos - Visualizar Datos Textuales
# Nube de palabras basada en sitio web del Proyecto de Ecologia de Tiburones de la Fundacion
# Charles Darwin
# URL: https://www.darwinfoundation.org/es/investigacion/proyectos/tiburones

# Bibliotecas -------------------------------------------------------------
library(tidyverse)
library(xml2)
library(rvest)
library(tidytext)
library(wordcloud2)

# Datos -------------------------------------------------------------------
#Accediendo a pagina web del proyecto donde trabajo actualmente
tiburonesFCD <- read_html("https://www.darwinfoundation.org/es/investigacion/proyectos/tiburones") %>%
  #Extrayendo informacion del cuerpo de la pagina web (basado en resultados de herramienta
  #SelectorGadget en Chrome)
  html_nodes("#sp-component") %>% 
  html_text()

#Extrayendo las palabras en el texto en si solamente, se ignora titulos, subtitulos y demas
EcoTibu_palabras <- tibble(texto = tiburonesFCD)

#Separando cada palabra
EcoTibu_palabras <- EcoTibu_palabras %>% 
  #separa cada palabra en una fila en el orde como aparecen
  #strip_numeric remueve los numeros en el texto
  unnest_tokens(palabra, texto, strip_numeric = T) %>% 
  #contando el numero de veces que aparece una palabra y ordenando de mayor a menor ocurrencia
  count(palabra, sort = T)

#Stop words son palabras que van a ser excluidas del analisis. Generalmente incluyen articulos,
#preposiciones, conjunciones, etc. Es buena practica explicar las razones por las que son excluidas.
stopwords_es <- read_csv("../Datos/stopwords.csv")
#Palabras adicionales a ser ignoradas del analisis
otras_stopwords <- tibble(palabra = c("salinas", "león", "marrero", "https", "doi.org", "shark",
                                      "acuña", "the", "of", "on", "in", "at", "et", "pelayo",
                                      "peerj", "first", "and", "hearn"))

#Excluir stop words del texto a ser analizado
EcoTibu_frecuencias <- EcoTibu_palabras %>% 
  anti_join(stopwords_es) %>% 
  anti_join(otras_stopwords)

# Graficos ----------------------------------------------------------------
#Nube de frecuencias con las palabras y su ocurrencia
g <- wordcloud2(EcoTibu_frecuencias)
#Guardando el grafico interactivo
htmlwidgets::saveWidget(g, file = paste0(getwd(),"/Outputs/Grafico22Texto.html"), selfcontained = F)
#Guardando grafico interactivo como png
webshot::webshot("Outputs/Grafico22Texto.html", "Outputs/Grafico22Texto.png", delay = 5)
