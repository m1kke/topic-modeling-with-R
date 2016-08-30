# Load libraries
library(tm)
library(topicmodels)
library(SnowballC)

# Load metadata and turn it into dataframe
# here

# Create variables
#dataDir <- "C:/Users/Mikko/Data Analysis Projects/topic-modeling-with-R/data"
#resultsDir <- "C:/Users/Mikko/Data Analysis Projects/topic-modeling-with-R/results"
dataDir <- "C:/Users/mikkok/Downloads/csc-dataproject/topic-modeling-with-R/data"
resultsDir <- "C:/Users/mikkok/Downloads/csc-dataproject/topic-modeling-with-R/results"
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
k <- 5
suomiStops <- c("aina", "alla", "ehkä", "eivät", "emme", "en", "enemmän", "ennen", "et", "että", "ette", "hän", "häneen", "hänellä", "hänelle", "häneltä", "hänen", "hänessä", "hänestä", "hänet", "häntä", "he", "heidän", "heidät", "heihin", "heillä", "heille", "heiltä", "heissä", "heistä", "heitä", "hlö", "hlöä", "oikein", "http", "hyvin", "ilman", "itse", "ja", "jälkeen", "johon", "joiden", "joihin", "joiksi", "joilla", "joille", "joilta", "joina", "joissa", "joista", "joita", "joka", "joka", "joksi", "jolla", "jolle", "jolta", "jona", "jonka", "jos", "jossa", "josta", "jota", "jotka", "kai", "kaikki", "kanssa", "kaukana", "keiden", "keihin", "keiksi", "keillä", "keille", "keiltä", "keinä", "keissä", "keistä", "keitä", "keneen", "keneksi", "kenellä", "kenelle", "keneltä", "kenen", "kenenä", "kenessä", "kenestä", "kenet", "kenties", "keskellä", "kesken", "ketä", "ketkä", "ketkä", "koska", "koskaan", "kuin", "kuinka", "kuka", "kun", "kyllä", "lähellä", "läpi", "liian", "lla", "luona", "me", "meidän", "meidät", "meihin", "meillä", "meille", "meiltä", "meissä", "meistä", "meitä", "mihin", "mikä", "miksi", "millä", "mille", "milloin", "milloinkaan", "miltä", "minä", "minkä", "minua", "minulla", "minulle", "minulta", "minun", "minussa", "minusta", "minut", "minuun", "missä", "mistä", "mitä", "miten", "mitkä", "mukaan", "mutta", "muut", "näiden", "näihin", "näiksi", "näillä", "näille", "näiltä", "näinä", "näissä", "näistä", "näitä", "nämä", "ne", "niiden", "niihin", "niiksi", "niillä", "niille", "niiltä", "niin", "niinä", "niissä", "niistä", "niitä", "noiden", "noihin", "noiksi", "noilla", "noille", "noilta", "noin", "noina", "noissa", "noista", "noita", "nopeasti", "nuo", "nyt", "oikea", "oikealla", "ole", "olemme", "olen", "olet", "olette", "oli", "olimme", "olin", "olisi", "olisimme", "olisin", "olisit", "olisitte", "olisivat", "olit", "olitte", "olivat", "olla", "olleet", "ollut", "on", "ovat", "paljon", "poikki", "puh", "saa", "saada", "se", "sekä", "sen", "siellä", "siihen", "siinä", "siitä", "siksi", "sillä", "sille", "siltä", "sinä", "sinua", "sinulla", "sinulle", "sinulta", "sinun", "sinussa", "sinusta", "sinut", "sinuun", "sitä", "ssa", "sta", "suoraan", "tähän", "tai", "takana", "takia", "täksi", "tällä", "tälle", "tältä", "tämä", "tämän", "tänä", "tässä", "tästä", "tätä", "te", "teidän", "teidät", "teihin", "teillä", "teille", "teiltä", "teissä", "teistä", "teitä", "tms", "tuo", "tuoda", "tuohon", "tuoksi", "tuolla", "tuolle", "tuolta", "tuon", "tuona", "tuossa", "tuosta", "tuota", "vaan", "vähän", "vähemmän", "vai", "vain", "vaikka", "vasen", "vasemmalla", "vastan", "vielä", "vieressä", "voi", "voida", "voit", "www", "yhdessä", "yli", "ylös", "yms", "com", "fax", "klo", "myös", "muuta", "viim", "asti", "sis", "koko", "alle", "joskus", "sivu", "paitsi", "sitten", "tule", "auki", "paras", "lue", "lisää", "joko", "ihan", "saat", "ei", "html")

# Set working dir to load data
setwd(dataDir)

# Get filenames
filenames <- list.files(dataDir)

# Load files into Corpus
docs <- Corpus(DirSource(getwd()))

# Convert to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove punctuation
docs <- tm_map(docs, removePunctuation)

# Remove special chars
removeSpecials <- function(x) gsub("[^0-9a-zA-ZäÄöÖåÅ ]", "", x)
docs <- tm_map(docs, removeSpecials)

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove stopwords
docs <- tm_map(docs, removeWords, stopwords("english")) #english
docs <- tm_map(docs, removeWords, suomiStops) #finnish by Heikki Hyppänen http://www.nettiapina.fi/wp-content/uploads/2007/04/fi_stopwords.txt
docs <- tm_map(docs, removeWords, stopwords("swedish")) #swedish

# Stem documents
docs <- tm_map(docs, stemDocument) #or perhaps snowballc

# Strip whitespace
docs <- tm_map(docs, stripWhitespace)

# Turn into plaintext document
docs <- tm_map(docs, PlainTextDocument)

# Create DTM
dtm <- DocumentTermMatrix(docs)
rownames(dtm) <- filenames

# Mining the corpus
freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq, decreasing = T)
freq[head(ord)]
freq[tail(ord)]
dtmr <- DocumentTermMatrix(docs, control = list(wordLengths = c(4, 20), bounds = list(global = c(3, 27))))
dtmr
freqr <- colSums(as.matrix(dtmr))
length(freqr)
ordr <- order(freqr, decreasing = T)
freqr[head(ordr)]
freqr[tail(ord)]

findFreqTerms(dtmr, lowfreq = 80)
findAssocs(dtmr, "koulutuksen", 0.6)
wf = data.frame(term=names(freqr), occurrences = freqr)
library(ggplot2)
p <- ggplot(subset(wf, freqr < 100), aes(term, occurrences))
p <- p + geom_bar(stat = "identity")
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

library(wordcloud)
set.seed(42)
wordcloud(names(freqr), freqr, min.freq = 50)
wordcloud(names(freqr), freqr, min.freq = 70, colors = brewer.pal(6, "Dark2"))

# Model topics
ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

# Change workdir to results folder
setwd(resultsDir)

ldaOut.topics <- as.matrix(topics(ldaOut)) #could be tuned

ldaOut.terms <- as.matrix(terms(ldaOut, 5))

topicProbabilities <- as.data.frame(ldaOut@gamma)

topic1ToTopic2 <- lapply(1:nrow(dtm), function(x) sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k - 1])

topic2ToTopic3 <- lapply(1:nrow(dtm), function(x) sort(topicProbabilities[x,])[k - 1]/sort(topicProbabilities[x,])[k - 2])


# Write results to CSVs
write.csv(ldaOut.topics, file = paste("LDA - K", k, "DocsToTopics.csv", sep = " "))

write.csv(ldaOut.terms, file = paste("LDA - K", k, "TopicsToTerms.csv"))

write.csv(topicProbabilities, file = paste("LDA - K", k, "TopicProbabilities.csv"))

write.csv(topic1ToTopic2, file = paste("LDA - K", k, "Topic1ToTopic2.csv"))

write.csv(topic2ToTopic3, file = paste("LDA - K", k, "Topic2ToTopic3.csv"))
