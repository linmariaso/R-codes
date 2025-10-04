library(gutenbergr)
library(tidyverse)
library(syuzhet)

var <- data.frame(gutenberg_metadata)
gutenberg_metadata %>%
  filter(title == "Don Quijote")
libros <- data.frame(gutenberg_metadata) %>% 
  filter(language == 'es')
don_quijote <- gutenberg_download(2000, strip = TRUE)
#Codificar
Encoding(don_quijote$text) <- 'latin1'
str(don_quijote)

#Reemplazar
don_quijote <- don_quijote$text %>%
  paste0(collapse = ' ') %>%
  gsub(pattern = '\',/',replacement = ' ')

#Dividir texto en oraciones
don_quijote_sentences <- get_sentences(don_quijote)
don_quijote_sentimientos_nrc <- get_sentiment(don_quijote_sentences,
                                          method = 'nrc',#nrc,bing,afinn,stadford
                                          language = 'spanish')
don_quijote_sentimientos_bing <- get_sentiment(don_quijote_sentences,
                                              method = 'bing',#nrc,bing,afinn,stadford
                                              language = 'spanish')
don_quijote_sentimientos_affin <- get_sentiment(don_quijote_sentences,
                                              method = 'afinn',#nrc,bing,afinn,stadford
                                              language = 'spanish')

par(mfrow=c(3,1))
plot(don_quijote_sentimientos_nrc,
     type = 'l',
     main = "Don Quijote nrc",
     xlab = "Tiempo narrativo",
     ylab = "Sentimiento")
plot(don_quijote_sentimientos_bing,
     type = 'l',
     main = "Don Quijote bing",
     xlab = "Tiempo narrativo",
     ylab = "Sentimiento")
plot(don_quijote_sentimientos_affin,
     type = 'l',
     main = "Don Quijote afinn",
     xlab = "Tiempo narrativo",
     ylab = "Sentimiento")

dev.off()

par(mar=c(1,1,1,1))
simple_plot(don_quijote_sentimientos_nrc)

par(mar=c(1,1,1,1))
simple_plot(don_quijote_sentimientos_bing)

par(mar=c(1,1,1,1))
simple_plot(don_quijote_sentimientos_affin)
