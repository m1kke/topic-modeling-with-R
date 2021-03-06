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
suomiStops <- c("aina", "alla", "ehk�", "eiv�t", "emme", "en", "enemm�n", "ennen", "et", "ett�", "ette", "h�n", "h�neen", "h�nell�", "h�nelle", "h�nelt�", "h�nen", "h�ness�", "h�nest�", "h�net", "h�nt�", "he", "heid�n", "heid�t", "heihin", "heill�", "heille", "heilt�", "heiss�", "heist�", "heit�", "hl�", "hl��", "oikein", "http", "hyvin", "ilman", "itse", "ja", "j�lkeen", "johon", "joiden", "joihin", "joiksi", "joilla", "joille", "joilta", "joina", "joissa", "joista", "joita", "joka", "joka", "joksi", "jolla", "jolle", "jolta", "jona", "jonka", "jos", "jossa", "josta", "jota", "jotka", "kai", "kaikki", "kanssa", "kaukana", "keiden", "keihin", "keiksi", "keill�", "keille", "keilt�", "kein�", "keiss�", "keist�", "keit�", "keneen", "keneksi", "kenell�", "kenelle", "kenelt�", "kenen", "kenen�", "keness�", "kenest�", "kenet", "kenties", "keskell�", "kesken", "ket�", "ketk�", "ketk�", "koska", "koskaan", "kuin", "kuinka", "kuka", "kun", "kyll�", "l�hell�", "l�pi", "liian", "lla", "luona", "me", "meid�n", "meid�t", "meihin", "meill�", "meille", "meilt�", "meiss�", "meist�", "meit�", "mihin", "mik�", "miksi", "mill�", "mille", "milloin", "milloinkaan", "milt�", "min�", "mink�", "minua", "minulla", "minulle", "minulta", "minun", "minussa", "minusta", "minut", "minuun", "miss�", "mist�", "mit�", "miten", "mitk�", "mukaan", "mutta", "muut", "n�iden", "n�ihin", "n�iksi", "n�ill�", "n�ille", "n�ilt�", "n�in�", "n�iss�", "n�ist�", "n�it�", "n�m�", "ne", "niiden", "niihin", "niiksi", "niill�", "niille", "niilt�", "niin", "niin�", "niiss�", "niist�", "niit�", "noiden", "noihin", "noiksi", "noilla", "noille", "noilta", "noin", "noina", "noissa", "noista", "noita", "nopeasti", "nuo", "nyt", "oikea", "oikealla", "ole", "olemme", "olen", "olet", "olette", "oli", "olimme", "olin", "olisi", "olisimme", "olisin", "olisit", "olisitte", "olisivat", "olit", "olitte", "olivat", "olla", "olleet", "ollut", "on", "ovat", "paljon", "poikki", "puh", "saa", "saada", "se", "sek�", "sen", "siell�", "siihen", "siin�", "siit�", "siksi", "sill�", "sille", "silt�", "sin�", "sinua", "sinulla", "sinulle", "sinulta", "sinun", "sinussa", "sinusta", "sinut", "sinuun", "sit�", "ssa", "sta", "suoraan", "t�h�n", "tai", "takana", "takia", "t�ksi", "t�ll�", "t�lle", "t�lt�", "t�m�", "t�m�n", "t�n�", "t�ss�", "t�st�", "t�t�", "te", "teid�n", "teid�t", "teihin", "teill�", "teille", "teilt�", "teiss�", "teist�", "teit�", "tms", "tuo", "tuoda", "tuohon", "tuoksi", "tuolla", "tuolle", "tuolta", "tuon", "tuona", "tuossa", "tuosta", "tuota", "vaan", "v�h�n", "v�hemm�n", "vai", "vain", "vaikka", "vasen", "vasemmalla", "vastan", "viel�", "vieress�", "voi", "voida", "voit", "www", "yhdess�", "yli", "yl�s", "yms", "com", "fax", "klo", "my�s", "muuta", "viim", "asti", "sis", "koko", "alle", "joskus", "sivu", "paitsi", "sitten", "tule", "auki", "paras", "lue", "lis��", "joko", "ihan", "saat", "ei", "html") # finnish stopwords
removeSpecials <- function(x) gsub("[^0-9a-zA-Z������ ]", "", x) # function to remove special chars
kNum <- 10 # number of topics

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
docs <- tm_map(docs, removeWords, suomiStops) # Remove finnish stopwords (by Heikki Hypp�nen http://www.nettiapina.fi/wp-content/uploads/2007/04/fi_stopwords.txt)
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
    headings[[o]] <- korpus[[o]]$meta$heading
  }
  
  #Turn headings into rownames
  rownames(dtm) <- headings
  
  #Topic Modeling
  for (k in 5:kNum) {
    x <- assign(paste0("ldaOut-", h, "-k", k), LDA(dtm, k, method="Gibbs", control=list(nstart=nstart, seed = seed, best = best, burnin = burnin, iter = iter, thin=thin)))
    for (t in 3:termNum) {
      x.topics <- assign(paste0("ldaOut-", h, "-k", k, ".topics"), as.matrix(topics(x)))
      x.terms <- assign(paste0("ldaOut-", h, "-k", k, "-t", t, ".terms"), as.matrix(terms(x, t)))
      write.csv(x.topics, file = paste("LDA - ", h, "-", k, "topics-", t, "terms - DocsToTopics.csv", sep = " "))
      write.csv(x.terms, file = paste("LDA - ", h, "-", k, "topics-", t, "terms - TopicsToTerms.csv"))
    }
  }
}

???library(igraph)

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