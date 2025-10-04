library(tidyverse)
library(tokenizers)
library(ggwordcloud)
library(RColorBrewer)
library(ggplot2)
text <- paste('También entiendo que como es temporada de elecciones, las expectativas para lo que lograremos este año son bajas. Aún así, señor Presidente de la Cámara de Representantes, aprecio el enfoque constructivo que usted y los otros líderes adoptaron a finales del año pasado para aprobar un presupuesto, y hacer permanentes los recortes de impuestos para las familias trabajadoras. Así que espero que este año podamos trabajar juntos en prioridades bipartidistas como la reforma de la justicia penal y ayudar a la gente que está luchando contra la adicción a fármacos de prescripción. Tal vez podamos sorprender de nuevo a los cínicos')
palabras <- tokenize_words(text)
length(palabras)
length(palabras[[1]])
tabla <- table(palabras[[1]])
tabla <- data_frame(palabra = names(tabla), recuento = as.numeric(tabla))
arrange(tabla,desc(recuento))
oraciones <- tokenize_sentences(text)
oraciones_words <- tokenize_words(oraciones[[1]])
length(oraciones_words[[1]])
length(oraciones_words[[2]])
length(oraciones_words[[3]])
length(oraciones_words[[4]])

#Analisis exploratorio
base_url <- "https://raw.githubusercontent.com/programminghistorian/jekyll/gh-pages/assets/basic-text-processing-in-r/"
url <- sprintf('%ssotu_text/236.txt',base_url)
texto <- paste(readLines(url), collapse = '\n')
palabra <- tokenize_words(texto)
tabla <- table(palabra[[1]])
tabla <- data_frame(word = names(tabla), count = as.numeric(tabla))
tabla <- arrange(tabla, desc(count))
palabras_frecuentes <- read_csv(sprintf('%s/%s', base_url,'word_frequency.csv'))
tabla <- inner_join(tabla, palabras_frecuentes)
palabras_menos_usadas <- filter(tabla, frequency<0.1)
wd <- data.frame(word=palabras_menos_usadas$word,
                 freq = as.numeric(palabras_menos_usadas$count))

str(wd)

wd <- wd[1:20,]
set.seed(1)

ggplot(wd, aes(label = word)) +
  geom_text_wordcloud() +
  theme_minimal()

ggwordcloud(words = wd$word,
            freq = wd$freq)

#Resumen del documento
print(filter(tabla,frequency< 0.05),n=15)

#Metadatos
metadatos <- read_csv(sprintf("%s/%s", base_url,'metadata.csv'))
#metadatos <- data.frame(filter(tabla,frequency<0.002))
#result <- c(metadatos$president[221], metadatos$year[221], tabla$word[1:5])
#paste(result, collapse = ';')
tabla <- filter(tabla,frequency< 0.005)
resul <- c(metadatos$president[236],metadatos$year[236],tabla$word[1:5])
paste(resul,collapse = ';')

archivo <- sprintf('%s/sotu_text/%03d.txt',base_url,1:236)
texto <- c()
for(f in archivo){
  texto <- c(texto, paste(readLines(f), collapse = '\n' ))
}
palabras <- tokenize_words(texto)
sapply(palabras,length)
qplot(metadatos$year,sapply(palabras,length)) +
  labs(x='año', y= 'Número de palabras')

qplot(metadatos$year,sapply(palabras,length), color = metadatos$sotu_type) +
  labs(x='año', y= 'Número de palabras') 

descripcion <- c()

for(i in 1:length(palabras)){
  tabla <- table(palabras[[i]])
  tabla <- data_frame(word=names(tabla),count = as.numeric(tabla))
  tabla <- arrange(tabla,desc(count))
  tabla <- inner_join(tabla,palabras_frecuentes)
  tabla <- filter(tabla, frequency< 0.002)
  resul <- c(metadatos$president[i],metadatos$year[i],tabla$word[1:5])
  descripcion <-  c(descripcion,paste(resul, collapse = '; '))
}

cat(descripcion,sep = '\n')

#palabra <- tokenize_words(texto)
#archivo <- sprintf('%s/sotu_text/%03d.txt',base_url,1:236)
#texto <- c()
#for (f in archivo) {
#    texto <- c(texto,paste(readLines(f),collapse = '\n'))
#}
#palabra <- tokenize_words(texto)
#sapply(palabra, length)
#qplot(metadatos$year, sapply(palabra,length)) +
#  labs(x = 'Año', y = 'Numero de palabras')

#descripcion <- c()

