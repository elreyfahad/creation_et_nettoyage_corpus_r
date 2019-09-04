library("wordcloud")
library("InspectChangepoint")
library("corrplot") 

# Mettre tout en minuscules
wiki.cl1 <- tm_map(corpus, content_transformer(tolower))
# Tuer les balises
wiki.cl2 <- tm_map(wiki.cl1, content_transformer(extractHTMLStrip))
# kill_chars est une fonction pour nettoyage personnalisé
(kill_chars <- content_transformer (function(x, pattern) gsub(pattern, " ", x)))
tm_map (wiki.cl2, kill_chars, "\u2019")
tm_map (wiki.cl2, kill_chars,"'")
tm_map (wiki.cl2, kill_chars,"[«»”“\"]")
tm_map (wiki.cl2, kill_chars,"\\[modifier\\]")

# enlever les ponctuations qui restent
wiki.cl3 <- tm_map (wiki.cl2, removePunctuation, preserve_intra_word_dashes = TRUE)

# Tuer les mots fréquents (stop words)
wiki.essence <- tm_map (wiki.cl3, removeWords, stopwords("french"))

# Extraire les racines. La bibliothèque SnowballC doit être installée.
getStemLanguages()
#Stemming
wiki.racines <- tm_map (wiki.essence, stemDocument, language="french")

# Enlever des blancs s'il en reste
wiki.racines <- tm_map (wiki.racines, stripWhitespace)

# Le corpus final nettoyé
corpus1<- wiki.racines

# test
corpus1 [[2]]
class(corpus1)



#generation du nuage de mots

dtm <- TermDocumentMatrix(corpus1)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)



set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#graphe des 10 mots le plus utlisé
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")


#graphe des 10 mots le plus utlisé
d_dernier=tail(d,10) #recuperation des 10 mots les moins utilisé

barplot(d_dernier$freq, las = 2, names.arg = d_dernier$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

#Matrice Terme documents
matrice_docs_termes <- DocumentTermMatrix(corpus1)
inspect(removeSparseTerms(matrice_docs_termes, 0.6))



# Créer une DTM avec des poids normalisées
mtd.norm <- as.matrix(removeSparseTerms(TermDocumentMatrix(corpus1, control=list(weighting=weightTf)),0.6))
corrplot (mtd.norm, is.corr=FALSE)
