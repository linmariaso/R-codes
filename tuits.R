library(tidyverse)
library(tidytext)
library(tm)
library(lubridate)
library(zoo)
library(scales)

#Informacion para visualizacion
tema_graf <- theme_minimal()+
  theme(text = element_text(family = 'serif'),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = '#EBEBEB', colour = NA),
        legend.position = 'none',
        legend.box.background = element_rect(fill = '#EBEBEB', colour = NA))

#Importar datos
download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/tuits_candidatos.csv",
              "tuits_candidatos.csv")
tuits <- read.csv("tuits_candidatos.csv", stringsAsFactors = FALSE,
                  fileEncoding = "latin1") %>%
  tbl_df()
tuits

#Traer la parte de sentimientos
download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv",
              "lexico_afinn.en.es.csv")
afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = FALSE,
                  fileEncoding = "latin1") %>%
  tbl_df()
afinn

#Preparar los datos
tuits <- tuits %>%
  separate(created_at, into = c('Fecha','Hora'), sep = " ") %>%
  separate(Fecha, into = c('Dia','Mes','Periodo'), sep = "/", remove = FALSE) %>%
  mutate(Fecha=dmy(Fecha),
         Semana=week(Fecha)%>% as.factor(),
         text = tolower(text))%>%
  filter(Periodo == c(2017,2016))

#Sacar palabras positivas y negativas
tuits_afinn <- tuits %>%
  unnest_tokens(input = 'text', output = 'Palabra')%>%
  inner_join(afinn, ., by = 'Palabra')%>%
  mutate(Tipo = ifelse(Puntuacion > 0, 'Positivo','Negativo'))%>%
  rename('Candidato' = screen_name)

#Quitar palabra no
tuits_afinn <- tuits_afinn%>%
  filter(Palabra != 'no')

#Agrupar por status
tuits <- tuits_afinn %>%
  group_by(status_id)%>%
  summarise(Puntuacion_tuit = mean(Puntuacion))%>%
  left_join(tuits, ., by = 'status_id')%>%
  mutate(Puntuacion_tuit = ifelse(is.na(Puntuacion_tuit),0,Puntuacion_tuit))%>%
  rename('Candidato' = screen_name)

#Contar tuits - agregar por cantidad de tuits por candidato
tuits_afinn %>%
  count(Candidato)

#Palabras distintas que se utilizan en los tuits
tuits_afinn %>%
  group_by(Candidato)%>%
  distinct(Palabra)%>%
  count()

#Plotear la informacion para revisar sentimientos
map(c('Positivo','Negativo'), function(sentimiento){
  tuits_afinn%>%
    filter(Tipo == sentimiento)%>%
    group_by(Candidato)%>%
    count(Palabra, sort = TRUE)%>%
    top_n(n=10, wt = n)%>%
    ggplot() +
    aes(Palabra,n,fill = Candidato) +
    geom_col() +
    facet_wrap('Candidato', scales = "free") +
    scale_y_continuous(expand = c(0,0)) +
    coord_flip() +
    labs(title = sentimiento) +
    tema_graf
})

#Analizar discursos entre positivos y negativos
tuits_afinn_fecha <- tuits_afinn %>%
  group_by(status_id)%>%
  mutate(Suma = mean(Puntuacion))%>%
  group_by(Candidato,Fecha)%>%
  summarise(media = mean(Puntuacion))

#Plotear tuits afinn fechas
tuits_afinn_fecha %>%
  ggplot()+
  aes(Fecha,media,color = Candidato) +
  geom_line() +
  tema_graf +
  theme(legend.position = 'top')

tuits_afinn_fecha %>%
  ggplot()+
  aes(Fecha,media,color = Candidato) +
  geom_hline(yintercept = 0, alpha = 0.35) +
  geom_line() +
  facet_grid(Candidato~.) +
  tema_graf +
  theme(legend.position = 'none')

#Porcentajes de Tuits positivos y negativos por fecha
tuits_afinn %>%
  group_by(Candidato, Fecha) %>%
  count(Tipo) %>%
  mutate(Proporcion = n/sum(n)) %>%
  ggplot() +
  aes(Fecha,Proporcion,fill = Tipo) +
  geom_col(width = 1) +
  facet_grid(Candidato~.) +
  scale_y_continuous(labels = percent_format()) +
  scale_x_date(expand = c(0,0)) +
  tema_graf +
  theme(legend.position = 'top')

#Proporcion general
tuits_afinn %>%
  count(Candidato, Tipo) %>%
  group_by(Candidato) %>%
  mutate(Proporcion = n/sum(n)) %>%
  ggplot() +
  aes(Candidato, Proporcion, fill = Tipo) +
  geom_col() +
  scale_y_continuous(labels = percent_format()) +
  tema_graf +
  theme(legend.position = 'top')
