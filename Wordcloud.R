library(ggwordcloud)

word <- thankyou_words_small
word

#Nube de palabras
set.seed(1)
ggplot(word, 
       aes(label = word,
           size = speakers)) +
  geom_text_wordcloud() +
  theme_minimal()

#
library(pacman)

#Biblioteca de BD orientada a texto
p_load('tm')
#Biblioteca para manipulacion de datos
p_load('tidyverse')
#Biblioteca para graficar nube de palabras
p_load('wordcloud')
#Paleta de colores para la nube de palabras
p_load('RColorBrewer')

url <- 'https://gist.github.com/EverVino/7bdbbe7ebdff5987970036f52f0e384f/raw/3a1997b6f9e3471555a941f8812ada0cef84977d/gistfile1.txt'
base <- read_file(url)
#vectorizacion del texto
base <- VCorpus(VectorSource(base),
                readerControl = list(reader = readPlain,
                                     language = 'es'))

#Transformar el texto (Minusculas)
base <- tm_map(base, tolower)
base <- base %>%
  tm_map(removePunctuation) %>% #remover signos de puntuacion
  tm_map(removeNumbers) %>% #remover numeros
  tm_map(removeWords, stopwords("Spanish")) #remover palabras que no esten en español

base <- tm_map(base, 
               removeWords,
               c("puede", "ser", "pues", "si", "aún","cómo"))

#Remover todos los espacios blancos
base <- tm_map(base, stripWhitespace)

#Cambiar a texto plano
base <- tm_map(base, PlainTextDocument)
#Tabla de frecuencias
tabla <- DocumentTermMatrix(base)

tabla <- cbind(palabras = tabla$dimnames$Terms,
               frecuencia = tabla$v)

str(tabla)
tabla <- as.data.frame(tabla)
str(tabla)
tabla$frecuencia <- as.numeric(tabla$frecuencia)

#Organizar de mayor a menor frecuencia
tabla <- tabla[order(tabla$frecuencia, decreasing = TRUE),]

tabla[1:20,] %>%
  ggplot(aes(x=palabras, y=frecuencia))+
  geom_bar(stat = "identity", color = 'black', fill = 'red')+
  coord_flip() + 
  labs(title = "20 palabras", x = "palabras", y = "frecuencia")
#Comando para borrar
dev.off()
wordcloud(tabla$palabras, 
          freq = tabla$frecuencia, 
          min.freq = 5,
          max.words = 20,
          random.order = FALSE,
          colors = brewer.pal(6,'Accent'))
