library(gutenbergr)
library(tidyverse)
library(syuzhet)

var <- data.frame(gutenberg_metadata)
gutenberg_metadata %>%
  filter(title == "La Divina Comedia")
libros <- data.frame(gutenberg_metadata) %>% 
  filter(language == 'es')
divina_comedia <- gutenberg_download(57303, strip = TRUE)
#Codificar
Encoding(divina_comedia$text) <- 'latin1'
str(divina_comedia)

#Reemplazar
divina_comedia <- divina_comedia$text %>%
  paste0(collapse = ' ') %>%
  gsub(pattern = '\',/',replacement = ' ')

#Dividir texto en oraciones
divina_comedia_sentences <- get_sentences(divina_comedia)
divina_comedia_sentimientosb <- get_sentiment(divina_comedia_sentences,
                                          method = 'bing',#nrc,bing,afinn,stadford
                                          language = 'spanish')
divina_comedia_sentimientosn <- get_sentiment(divina_comedia_sentences,
                                              method = 'nrc',#nrc,bing,afinn,stadford
                                              language = 'spanish')
divina_comedia_sentimientosa <- get_sentiment(divina_comedia_sentences,
                                              method = 'afinn',#nrc,bing,afinn,stadford
                                              language = 'spanish')
par(mfrow=c(3,1))
plot(divina_comedia_sentimientosn,
     type = 'l',
     main = "Divina Comedia nrc",
     xlab = "Tiempo narrativo",
     ylab = "Sentimiento")
plot(divina_comedia_sentimientosb,
     type = 'l',
     main = "Divina Comedia Bing",
     xlab = "Tiempo narrativo",
     ylab = "Sentimiento")
plot(divina_comedia_sentimientosa,
     type = 'l',
     main = "Divina Comedia Afinn",
     xlab = "Tiempo narrativo",
     ylab = "Sentimiento")

par(mar=c(1,1,1,1))
simple_plot(divina_comedia_sentimientosn)

par(mar=c(1,1,1,1))
simple_plot(divina_comedia_sentimientosb)

par(mar=c(1,1,1,1))
simple_plot(divina_comedia_sentimientosa)
