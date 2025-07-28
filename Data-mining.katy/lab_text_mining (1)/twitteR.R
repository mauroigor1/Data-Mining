###Text mining con datos de twitter###
## Opcion 1: Descargar datos de twitter
##instalando las librerias que permiten conectarse con twitter##
install.packages("twitteR", dependencies = TRUE)
install.packages("ROAuth", dependencies = TRUE)

##Cargando las librerias##
library(twitteR)
library(ROAuth)
library(twitteR)
library(RCurl)
library(stringr)

## Autentificacion en twitter##
consumer_key <- '5GRib0s6FGCDaV47oxnQpY6RF'
consumer_secret <- 'oFpw4aFlswsNoP4jwaxXXFNKDfH3n5B9O4zSwnuaG5s1wRd0zD'
access_token <- '381435706-RnY8gfbmRK7sYOTfdVh60nn7KNOJCyehQ3QDFJr3'
access_secret <- 'W2M6vGfqeOQJp6rpxGrR5c4ospzeA11VR02ltN3maMfmD'
setup_twitter_oauth(consumer_key, consumer_secret, access_token,
                    access_secret)
##Extraccion de 3200 twitts
tweets <- searchTwitter("#CopaConfederaciones" ,n=3200)
#tweets <- userTimeline("BioBioChile", n = 3200)
#Busca los tweets del usuario

##Contando el numero de twitts cargados
(n.tweet <- length(tweets))

# arreglo de twitts a dataframe
tweets.df <- twListToDF(tweets)
# explorando el tweet #190
tweets.df[200, c("id", "created", "screenName", "replyToSN",
                 "favoriteCount", "retweetCount", "longitude", "latitude", "text")]

# imprimiendo el twitt 190 en el ancho de la pantalla
writeLines(strwrap(tweets.df$text[190], 60))

library(tm)
# Construiremos un corpus con los twitts descargados
myCorpus <- Corpus(VectorSource(tweets.df$text))
myCorpus[[200]]
# removiendo los emojis
#myCorpus <- tm_map(myCorpus, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))
myCorpus[[200]]
# convirtiendo todo a minusculas
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myCorpus[[200]]
# removiendo URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
myCorpus[[200]]
# removiendo cualquier otro simbolo raro
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
myCorpus[[200]]
# removiendo stopwords
myCorpus <- tm_map(myCorpus, removeWords, stopwords("spanish"))
myCorpus[[200]]
# removiendo espacios en blanco extras (pudieron quedar ahi luego de remover los stopwords
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpus[[200]]
# guardaremos una copia para realizar otros analisis despues
myCorpusCopy <- myCorpus
# Nos aseguramos que el corpus es texto plano
myCorpus <- tm_map(myCorpus, PlainTextDocument)
#Vamos ahora a cargar el paquete SnowballC para hacer Stemming, esto es borrar los sufijos de las palabras
#para dejarlas en su raiz 
#install.packages("SnowballC", dependencies = TRUE)
library(SnowballC)
myCorpus <- tm_map(myCorpus, stemDocument)
# Quitamos los espacios que han salido con tanto retoque
myCorpus <- tm_map(myCorpus, stripWhitespace)

#Completacion de stem (esto es volver al principio)
#stemCompletion2 <- function(x, dictionary) {
#  x <- unlist(strsplit(as.character(x), " "))
#  x <- x[x != ""]
#  x <- stemCompletion(x, dictionary=dictionary) 
#  x <- paste(x, sep="", collapse=" ") 
#  PlainTextDocument(stripWhitespace(x))
#}
#myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
myCorpus <- Corpus(VectorSource(myCorpus))
#writeLines(strwrap(myCorpus[[1]]$content, 60))

#contando la frecuencia de cada palabra
wordFreq <- function(corpus, word) { results <- lapply(corpus,
                                                       function(x) { grep(as.character(x), pattern=paste0("\\<",word)) } )
}
tdm <- TermDocumentMatrix(myCorpus,
                          control = list(wordLengths = c(1, Inf)))
tdm

# inspeccionando las palabras mas frecuentes
(freq.terms <- findFreqTerms(tdm, lowfreq = 100))

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 100)
df <- data.frame(term = names(term.freq), freq = term.freq)

#grafico de frecuencias
library(ggplot2)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))

m <- as.matrix(tdm)
# ordenando las frecuencias en orden decreciente 
word.freq <- sort(rowSums(m), decreasing = T)

install.packages("wordcloud", dependencies = TRUE)
# colores
pal <- brewer.pal(9, "BuGn")[-(1:4)]

# graficando el word cloud

library(wordcloud)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 70,
          random.order = F, colors = pal)
which(names(word.freq)==c('rt'))
word.freq[1]
word.freq[2]
# Sin las palabras cuentapublica y rt
wordcloud(words = names(word.freq)[which(names(word.freq)!=c('rt'))], freq = word.freq[-c(1,2)], min.freq = 70,
          random.order = F, colors = pal)

# que palabras estan asociadas con 'presidenta'? 
#findAssocs(tdm, "presidenta", 0.2)

# que palabras estan asociadas con  'ser'? 
#findAssocs(tdm, "mitad", 0.2)

#Graficando las asociaciones
#install.packages('graph', dependencies = TRUE)
#install.packages('Rgraphviz', dependencies = TRUE)
#library(graph)
#library(Rgraphviz)
plot(tdm, term = freq.terms, corThreshold = 0.1, weighting = T)

#construyendo el modelo de topicos
install.packages('topicmodels', dependencies = TRUE)
library(topicmodels)
dtm <- as.DocumentTermMatrix(tdm)
lda <- LDA(dtm, k = 8) # 8 topicos mas importantes
term <- terms(lda, 7) # primeros siete terminos en cada topico 
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))

topics <- topics(lda) # primer topico identificado en cada twitt
topics <- data.frame(date=as.Date(tweets.df$created), topic=topics) 

#Grafica de los topicos
ggplot(topics, aes(date, fill = term[topic])) +
geom_density(position = "stack")

##analisis de sentimientos
#############Dificil en espanol, librerias no disponibles en MAC o en la ultima version de R###########
# instalando paquetes
require(devtools)
install_github("sentiment140", "okugami79")

# analisis de sentimientos
#install.packages('sentiment', dependencies = TRUE)
library(sentiment)
sentiments <- sentiment(tweets.df$text)
table(sentiments$polarity)

# grafico de sentimientos
sentiments$score <- 0
sentiments$score[sentiments$polarity == "positive"] <- 1
sentiments$score[sentiments$polarity == "negative"] <- -1
sentiments$date <- as.Date(tweets.df$created)
result <- aggregate(score ~ date, data = sentiments, sum)
plot(result, type = "l")
######################################################################################################

# Estudiando la estructura de la red
user <- getUser("biobio")
user$toDataFrame()
friends <- user$getFriends() # a quien sigue este usuario
followers <- user$getFollowers() # quien sigue a este usuario
followers2 <- followers[[1]]$getFollowers() # seguidores de seguidores

# seleccionar sus twitts mas populares
##Extraccion de 3200 twitts
tweets <- userTimeline("biobio", n = 3200)

##Contando el numero de twitts cargados
(n.tweet <- length(tweets))

# arreglo de twitts a dataframe
tweets.df <- twListToDF(tweets)
table(tweets.df$retweetCount)
selected <- which(tweets.df$retweetCount >= 9)
# graficando los twitts mas populates
dates <- strptime(tweets.df$created, format="%Y-%m-%d")
plot(x=dates, y=tweets.df$retweetCount, type="l", col="grey",
     xlab="Date", ylab="Times retweeted")
colors <- rainbow(10)[1:length(selected)]
points(dates[selected], tweets.df$retweetCount[selected],
       pch=19, col=colors)
text(dates[selected], tweets.df$retweetCount[selected],
     tweets.df$text[selected], col=colors, cex=.9)

retweeters(tweets[[1]]$id)
retweets(tweets[[1]]$id)

