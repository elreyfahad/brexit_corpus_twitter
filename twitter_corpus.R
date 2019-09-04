#install.packages("twitteR")
#install.packages("ROAuth")
#install.packages("syuzhet")
#install.packages("topicmodels")


library("NLP")
library("twitteR")
#le package "tm" propose un certain nombre de fonctions intéressantes pour le retravail de données textuelles
library("tm") 
library("SnowballC")
library("stringi")
library("topicmodels")

library("ROAuth")
library("wordcloud")


consumer_key <- 'r1nTeZaXHwfjXKARyaZIVurc9'
consumer_secret <- 'EikApVxGDxiybnugl0hDfpPhtuloCjDO43yAJMoXlfY3BBnDya'
access_token <- '2598381610-GXnmHSk6RjqTCM3g795sSCWoU2ZobSgEDHZlEGG'
access_secret <- 'WTcf5ryAS7G0PFgWei9W06jHuYRbSKJSGPRv3knZOY0iS'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


#extraction des tweets

tweets_got <- searchTwitter("#Brexit", n=500,lang = "en")

#suppression de Retweets
tweets_got = strip_retweets(tweets_got)

#cette fonction va permettre de transformer la liste de tweets extraite en un "dataframe"
got_tweets <- twListToDF(tweets_got)
str(got_tweets) #description du Dataframe
#View(got_tweets) #visualisation du dataframe





got_text<-got_tweets$text #recuperation du text des tweets



tweets_text <- stri_trans_general(got_text, "lower") # on va ici mettre tous les mots en minuscules

# on crée ci-dessous une fonction R qui va nous permettre de faire un peu de nettoyage de nos données : 
# suppression des liens web et d'une liste de certains mots (contenue dans "mots")

myCleaningFunction <- function(x, mots) {
  
  s <- x
  #regex pour supprimer les liens https qui se trouvent dans les text
  s <- gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", s, perl = TRUE)
  
  #suppression des points
  s <- gsub("[\\.]*","",s,perl=TRUE)
  
  #suppression des mentions
  s <- gsub("@[a-zA-Z0-9]*","",s,perl=TRUE)
  
  s <- gsub("[\\W]+"," ",s,perl=TRUE)
  
  
  
  
  #suppression de certains mots qu'on a passe en parametre
  for(k in 1:length(mots)) {
    
    s <- gsub(mots[k], "", s, perl = TRUE)
    
  }
  
  return(s)
  
}

# vecteur de mots à supprimer,on va supprimer les stopword en anglais
mots_supp <- c("brexit")
tweets_text <- sapply(tweets_text, myCleaningFunction, mots_supp)
names(tweets_text) <- NULL


#creation d'un nouveau data frame ayant comme colonne ,l'id du tweet
#le text normalisé du tweet,la date et l'auteur du tweets

donne=data.frame( id=got_tweets$id,tweet=tweets_text,
                       date=got_tweets$created,username=got_tweets$screenName)

View(donne) #visualisation du dataframe

# creation d'un fichier CSV  a partir de ce data frame

write.csv2(donne, file = "brexit_tweets.csv",append = TRUE, row.names = FALSE)











#Creation d'un corpus a partir de la colonne "tweet" (le text des tweet) du dataframe "donne" 
tweets_corpus <- Corpus(VectorSource(donne$tweet))

# ici cela va supprimer automatiquement tous les caractères de ponctuation
tweets_corpus <- tm_map(tweets_corpus, removePunctuation)

# ici cela va supprimer automatiquement une bonne partie des mots anglais basiques
tweets_corpus <- tm_map(tweets_corpus, removeWords, stopwords("en")) 

# ici cela va supprimer automatiquement tous les espaces vides
tweets_corpus <- tm_map(tweets_corpus, stripWhitespace) 

tweets_text<-tweets_corpus


dtm <- TermDocumentMatrix(tweets_corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v),freq = v)

head(d, 20) 
barplot(d[1:20,]$freq, las = 2, names.arg = d[1:20,]$word,
        col ="lightblue", main ="Mots les plus fréquents",
        ylab = "Fréquences")

set.seed(123456)

#nuage de mots
wordcloud(tweets_corpus, max.words = 200, colors = brewer.pal(8, "Dark2"))

