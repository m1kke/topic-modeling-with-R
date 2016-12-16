# Load libraries
library(tm) # for text mining
library(topicmodels) # for for topic modeling
library(SnowballC) # for stemming
library(textcat) # for language detection
library(wordcloud) # for wordclouds


# Create variables
#dataDir <- "C:/Users/Mikko/Desktop/topic-modeling-with-R/data"
#resultsDir <- "C:/Users/Mikko/Desktop/topic-modeling-with-R/results"
dataDir <- "C:/Users/mikkok/Desktop/CSC projekti/topic-modeling-with-R/data"
resultsDir <- "C:/Users/mikkok/Desktop/CSC projekti/topic-modeling-with-R//results"
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
t <- 5 # number of terms per topic
suomiStops <- c("aina", "alla", "ehkä", "eivät", "emme", "en", "enemmän", "ennen", "et", "että", "ette", "hän", "häneen", "hänellä", "hänelle", "häneltä", "hänen", "hänessä", "hänestä", "hänet", "häntä", "he", "heidän", "heidät", "heihin", "heillä", "heille", "heiltä", "heissä", "heistä", "heitä", "hlö", "hlöä", "oikein", "http", "hyvin", "ilman", "itse", "ja", "jälkeen", "johon", "joiden", "joihin", "joiksi", "joilla", "joille", "joilta", "joina", "joissa", "joista", "joita", "joka", "joka", "joksi", "jolla", "jolle", "jolta", "jona", "jonka", "jos", "jossa", "josta", "jota", "jotka", "kai", "kaikki", "kanssa", "kaukana", "keiden", "keihin", "keiksi", "keillä", "keille", "keiltä", "keinä", "keissä", "keistä", "keitä", "keneen", "keneksi", "kenellä", "kenelle", "keneltä", "kenen", "kenenä", "kenessä", "kenestä", "kenet", "kenties", "keskellä", "kesken", "ketä", "ketkä", "ketkä", "koska", "koskaan", "kuin", "kuinka", "kuka", "kun", "kyllä", "lähellä", "läpi", "liian", "lla", "luona", "me", "meidän", "meidät", "meihin", "meillä", "meille", "meiltä", "meissä", "meistä", "meitä", "mihin", "mikä", "miksi", "millä", "mille", "milloin", "milloinkaan", "miltä", "minä", "minkä", "minua", "minulla", "minulle", "minulta", "minun", "minussa", "minusta", "minut", "minuun", "missä", "mistä", "mitä", "miten", "mitkä", "mukaan", "mutta", "muut", "näiden", "näihin", "näiksi", "näillä", "näille", "näiltä", "näinä", "näissä", "näistä", "näitä", "nämä", "ne", "niiden", "niihin", "niiksi", "niillä", "niille", "niiltä", "niin", "niinä", "niissä", "niistä", "niitä", "noiden", "noihin", "noiksi", "noilla", "noille", "noilta", "noin", "noina", "noissa", "noista", "noita", "nopeasti", "nuo", "nyt", "oikea", "oikealla", "ole", "olemme", "olen", "olet", "olette", "oli", "olimme", "olin", "olisi", "olisimme", "olisin", "olisit", "olisitte", "olisivat", "olit", "olitte", "olivat", "olla", "olleet", "ollut", "on", "ovat", "paljon", "poikki", "puh", "saa", "saada", "se", "sekä", "sen", "siellä", "siihen", "siinä", "siitä", "siksi", "sillä", "sille", "siltä", "sinä", "sinua", "sinulla", "sinulle", "sinulta", "sinun", "sinussa", "sinusta", "sinut", "sinuun", "sitä", "ssa", "sta", "suoraan", "tähän", "tai", "takana", "takia", "täksi", "tällä", "tälle", "tältä", "tämä", "tämän", "tänä", "tässä", "tästä", "tätä", "te", "teidän", "teidät", "teihin", "teillä", "teille", "teiltä", "teissä", "teistä", "teitä", "tms", "tuo", "tuoda", "tuohon", "tuoksi", "tuolla", "tuolle", "tuolta", "tuon", "tuona", "tuossa", "tuosta", "tuota", "vaan", "vähän", "vähemmän", "vai", "vain", "vaikka", "vasen", "vasemmalla", "vastan", "vielä", "vieressä", "voi", "voida", "voit", "www", "yhdessä", "yli", "ylös", "yms", "com", "fax", "klo", "myös", "muuta", "viim", "asti", "sis", "koko", "alle", "joskus", "sivu", "paitsi", "sitten", "tule", "auki", "paras", "lue", "lisää", "joko", "ihan", "saat", "ei", "html") # finnish stopwords
removeSpecials <- function(x) gsub("[^0-9a-zA-ZäÄöÖåÅ ]", "", x) # function to remove special chars
k <- 4 # number of topics

# Ladataan metadata-excel
metaDir <- "C:/Users/mikkok/Desktop/CSC projekti/topic-modeling-with-R/"
setwd(metaDir)

library(xlsx)
metadata <- read.xlsx("aineisto-metadata.xlsx", sheetName = "Sheet1", header = TRUE, as.data.frame = T, encoding = "UTF-8", colClasses = c("integer", "character", "character", "integer", "character", "Date", "character", "character", "character", "character"))

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
  x <- assign(paste0(h, "-k", k), LDA(dtm, k, method="Gibbs", control=list(nstart=nstart, seed = seed, best = best, burnin = burnin, iter = iter, thin=thin)))
 
  x.topics <- assign(paste0(h, "-k", k, ".topics"), as.matrix(topics(x)))
  x.terms <- assign(paste0(h, "-k", k, "-t", t, ".terms"), as.matrix(terms(x, t)))
  write.csv(x.topics, file = paste(h, "-", k, "topics -", t, "terms - Dokumentit ja teemat.csv", sep = " "))
  write.csv(x.terms, file = paste(h, "-", k, "topics -", t, "terms - Teemojen termit.csv"))
  
  x.topicProbabilities <- as.data.frame(x@gamma)
  write.csv(x.topicProbabilities, file = paste(h, "-", k, "topics -", t, "terms - TopicProbabilities.csv"))
  
  x.topic1ToTopic2 <- lapply(1:nrow(dtm), function(x) sort(x.topicProbabilities[x,])[k]/sort(x.topicProbabilities[x,])[k - 1])
  x.topic2ToTopic3 <- lapply(1:nrow(dtm), function(x) sort(x.topicProbabilities[x,])[k - 1]/sort(x.topicProbabilities[x,])[k - 2])
  write.csv(x.topic1ToTopic2, file = paste(h, "-", k, "topics -", t, "terms - Topic1ToTopic2.csv"))
  write.csv(x.topic2ToTopic3, file = paste(h, "-", k, "topics -", t, "terms - Topic2ToTopic3.csv"))
  
  # Luodaan dataframe, johon tallennetaan dokumentin nimi ja todennäköisyydet, tallennetaan se muuttujaan
  df <- as.data.frame(x@documents)
  colnames(df) <- "Dokumentti"
  for (i in 1:length(x@documents)) {
    df$P1[i] = x@gamma[i,1]
    df$P2[i] = x@gamma[i,2]
    df$P3[i] = x@gamma[i,3]
    df$P4[i] = x@gamma[i,4]
  }
  assign(paste0(h, "_df"), df)
  
  # Luo sanapilvi jokaisesta kolumnista eli topikista
  # kolumni muutetaan listaksi, josta sanapilvi luodaan
  for (sarake in 1:ncol(x.terms)) {
    png(filename=paste0("Sanapilvi - ", h, " - topic ", sarake, ".png"), width=12, height=8, units="in", res=300)
    wordcloud(assign(paste0("sarake", sarake), x.terms[,sarake]), random.order=FALSE, colors=brewer.pal(8, "Dark2"), rot.per = 0)
    dev.off()
  }
  
  #posterior(`finnish.corp-k4`)$topics
  #posterior(`finnish.corp-k4`)$terms
}

# Yhdistä topic + probability data framet
topicprob <- rbind(finnish.corp_df, english.corp_df, swedish.corp_df)
write.csv(topicprob, file = "Kaikki kielet - Dokumentit ja todennäköisyydet.csv")

# Tallenna yksittäiset topicprobit
write.csv(english.corp_df, file = "English - Dokumentit ja todennäköisyydet.csv")
write.csv(finnish.corp_df, file = "Finnish - Dokumentit ja todennäköisyydet.csv")
write.csv(swedish.corp_df, file = "Swedish - Dokumentit ja todennäköisyydet.csv")

# Yhdistä topicprob dokumenttien metadataan
full_metadata <- merge(x = metadata, y = topicprob, by.x = "tiedostonnimi.txt", by.y = "Dokumentti", all.x = T)

# Tallenna täysi metadata
write.xlsx(full_metadata, "Full Metadata (unclean).xlsx")





