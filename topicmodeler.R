# Load libraries
library(tm) # for text mining
library(topicmodels) # for for topic modeling
library(SnowballC) # for stemming
library(textcat) # for language detection
library(wordcloud) # for wordclouds

# Create variables
dataDir <- "C:/Users/mikkok/Desktop/topic-modeling-with-R/data"
resultsDir <- "C:/Users/mikkok/Desktop/topic-modeling-with-R/results"
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
termNum <- 7 # number of terms per topic
suomiStops <- c("aina", "alla", "ehkä", "eivät", "emme", "en", "enemmän", "ennen", "et", "että", "ette", "hän", "häneen", "hänellä", "hänelle", "häneltä", "hänen", "hänessä", "hänestä", "hänet", "häntä", "he", "heidän", "heidät", "heihin", "heillä", "heille", "heiltä", "heissä", "heistä", "heitä", "hlö", "hlöä", "oikein", "http", "hyvin", "ilman", "itse", "ja", "jälkeen", "johon", "joiden", "joihin", "joiksi", "joilla", "joille", "joilta", "joina", "joissa", "joista", "joita", "joka", "joka", "joksi", "jolla", "jolle", "jolta", "jona", "jonka", "jos", "jossa", "josta", "jota", "jotka", "kai", "kaikki", "kanssa", "kaukana", "keiden", "keihin", "keiksi", "keillä", "keille", "keiltä", "keinä", "keissä", "keistä", "keitä", "keneen", "keneksi", "kenellä", "kenelle", "keneltä", "kenen", "kenenä", "kenessä", "kenestä", "kenet", "kenties", "keskellä", "kesken", "ketä", "ketkä", "ketkä", "koska", "koskaan", "kuin", "kuinka", "kuka", "kun", "kyllä", "lähellä", "läpi", "liian", "lla", "luona", "me", "meidän", "meidät", "meihin", "meillä", "meille", "meiltä", "meissä", "meistä", "meitä", "mihin", "mikä", "miksi", "millä", "mille", "milloin", "milloinkaan", "miltä", "minä", "minkä", "minua", "minulla", "minulle", "minulta", "minun", "minussa", "minusta", "minut", "minuun", "missä", "mistä", "mitä", "miten", "mitkä", "mukaan", "mutta", "muut", "näiden", "näihin", "näiksi", "näillä", "näille", "näiltä", "näinä", "näissä", "näistä", "näitä", "nämä", "ne", "niiden", "niihin", "niiksi", "niillä", "niille", "niiltä", "niin", "niinä", "niissä", "niistä", "niitä", "noiden", "noihin", "noiksi", "noilla", "noille", "noilta", "noin", "noina", "noissa", "noista", "noita", "nopeasti", "nuo", "nyt", "oikea", "oikealla", "ole", "olemme", "olen", "olet", "olette", "oli", "olimme", "olin", "olisi", "olisimme", "olisin", "olisit", "olisitte", "olisivat", "olit", "olitte", "olivat", "olla", "olleet", "ollut", "on", "ovat", "paljon", "poikki", "puh", "saa", "saada", "se", "sekä", "sen", "siellä", "siihen", "siinä", "siitä", "siksi", "sillä", "sille", "siltä", "sinä", "sinua", "sinulla", "sinulle", "sinulta", "sinun", "sinussa", "sinusta", "sinut", "sinuun", "sitä", "ssa", "sta", "suoraan", "tähän", "tai", "takana", "takia", "täksi", "tällä", "tälle", "tältä", "tämä", "tämän", "tänä", "tässä", "tästä", "tätä", "te", "teidän", "teidät", "teihin", "teillä", "teille", "teiltä", "teissä", "teistä", "teitä", "tms", "tuo", "tuoda", "tuohon", "tuoksi", "tuolla", "tuolle", "tuolta", "tuon", "tuona", "tuossa", "tuosta", "tuota", "vaan", "vähän", "vähemmän", "vai", "vain", "vaikka", "vasen", "vasemmalla", "vastan", "vielä", "vieressä", "voi", "voida", "voit", "www", "yhdessä", "yli", "ylös", "yms", "com", "fax", "klo", "myös", "muuta", "viim", "asti", "sis", "koko", "alle", "joskus", "sivu", "paitsi", "sitten", "tule", "auki", "paras", "lue", "lisää", "joko", "ihan", "saat", "ei", "html") # finnish stopwords
removeSpecials <- function(x) gsub("[^0-9a-zA-ZäÄöÖåÅ ]", "", x) # function to remove special chars
kNum <- 5 # number of topics

# Set working dir to load data
setwd(dataDir)

# Remove empty documents so they don't cause problems later
a <- dir(dataDir)
emptya <- a[file.info(a)[["size"]] == 0]
unlink(emptya)

#Get filenames
filenames <- list.files(dataDir)

# Load files into Corpus
docs <- Corpus(DirSource(getwd()))

# Clean data
docs <- tm_map(docs, content_transformer(tolower)) # Convert to lower case
docs <- tm_map(docs, removePunctuation) # Remove punctuation
docs <- tm_map(docs, removeSpecials) # Remove special chars
docs <- tm_map(docs, removeNumbers) # Remove numbers
docs <- tm_map(docs, removeWords, stopwords("english")) # Remove english stopwords
docs <- tm_map(docs, removeWords, suomiStops) # Remove finnish stopwords (by Heikki Hyppänen http://www.nettiapina.fi/wp-content/uploads/2007/04/fi_stopwords.txt)
docs <- tm_map(docs, removeWords, stopwords("swedish")) # Remove swedish stopwords
docs <- tm_map(docs, stemDocument) # Stem documents
docs <- tm_map(docs, stripWhitespace) # Strip whitespace
docs <- tm_map(docs, PlainTextDocument) # Turn into plaintext document

# Detect document language and save it into the metadata
# unique used for weird bug in swedish
for (i in 1:length(docs)) {
  rowname <- sort(table(textcat(docs[[i]]$content)), decreasing=T)
  if (!is.table(rowname)) {
    rowname <- attr(rowname, "names")
    docs[[i]]$meta$language = rowname
  }
  else {
    docs[[i]]$meta$language = rownames(rowname)[1]
  }
}

# Loop through documents to list all found languages, extract unique languages
lanlist <- list()
for (i in 1:length(docs)) { lanlist[i] <- docs[[i]]$meta$language }
unilist <- unique(lanlist)

# Save filenames in heading metadata
for (doc in 1:length(docs)) {
  docs[[doc]]$meta$heading <- filenames[doc]
}

# Create a corpus for each unique language
corplist <- list()
for (j in 1:length(unilist)){
  assign(paste0(unilist[j], ".corp"), tm_filter(docs, FUN = function(x) meta(x)[["language"]] == unilist[j]))
  corplist[j] <- paste0(unilist[j], ".corp")
}

# Change workdir to results folder
setwd(resultsDir)

# Topic modeling
for(h in corplist) {
  korpus = get(h)
  dtm <- DocumentTermMatrix(korpus)
  
  # Create headings list
  headings <- vector(mode="character", length = length(korpus))
  for (o in 1:length(korpus)) {
    headings[[o]] <- korpus[[o]]$meta$heading #wtf why double brackets!?
  }
  
  #Turn headings into rownames
  rownames(dtm) <- headings
  
  #Topic Modeling
  for (k in 2:kNum) {
    x <- assign(paste0("ldaOut-", h, "-k", k), LDA(dtm, k, method="Gibbs", control=list(nstart=nstart, seed = seed, best = best, burnin = burnin, iter = iter, thin=thin)))
    for (t in 3:termNum) {
      x.topics <- assign(paste0("ldaOut-", h, "-k", k, ".topics"), as.matrix(topics(x)))
      x.terms <- assign(paste0("ldaOut-", h, "-k", k, "-t", t, ".terms"), as.matrix(terms(x, t)))
      write.csv(x.topics, file = paste("LDA - ", h, "-", k, "topics-", t, "terms - DocsToTopics.csv", sep = " "))
      write.csv(x.terms, file = paste("LDA - ", h, "-", k, "topics-", t, "terms - TopicsToTerms.csv"))
      # luo jokaiselle termimatriisin kolumnille oma wordcloud ja sijoita se gridiin, tallenna grid kuvaksi tai pdf:ksi
    }
  }
}


# Remove empty documents from corpus to prevent errors during modeling
#rows.sum = apply(dtm, 1, sum)
#dtm = dtm[rows.sum > 0, ] # tai dtm[rows.sum > 0, ]

# Calculate probabilities
topicProbabilities <- as.data.frame(ldaOut@gamma)
topic1ToTopic2 <- lapply(1:nrow(dtm), function(x) sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k - 1])
topic2ToTopic3 <- lapply(1:nrow(dtm), function(x) sort(topicProbabilities[x,])[k - 1]/sort(topicProbabilities[x,])[k - 2])

# Write results to CSVs
write.csv(topicProbabilities, file = paste("LDA - K", k, "TopicProbabilities.csv"))
write.csv(topic1ToTopic2, file = paste("LDA - K", k, "Topic1ToTopic2.csv"))
write.csv(topic2ToTopic3, file = paste("LDA - K", k, "Topic2ToTopic3.csv"))