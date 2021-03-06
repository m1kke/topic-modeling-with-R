# Load libraries
library(tm) # for text mining
library(topicmodels) # for for topic modeling
library(SnowballC) # for stemming
library(textcat) # for language detection
library(wordcloud) # for wordclouds
library(XLConnect) # for reading excel (xlsx doesnt get dates right)

# Create variables
baseDir <- "C:/Users/Mikko/Data Analysis Projects/topic-modeling-with-R"
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
t <- 7 # number of terms per topic
suomiStops <- c("aina", "alla", "ehk�", "eiv�t", "emme", "en", "enemm�n", "ennen", "et", "ett�", "ette", "h�n", "h�neen", "h�nell�", "h�nelle", "h�nelt�", "h�nen", "h�ness�", "h�nest�", "h�net", "h�nt�", "he", "heid�n", "heid�t", "heihin", "heill�", "heille", "heilt�", "heiss�", "heist�", "heit�", "hl�", "hl��", "oikein", "http", "hyvin", "ilman", "itse", "ja", "j�lkeen", "johon", "joiden", "joihin", "joiksi", "joilla", "joille", "joilta", "joina", "joissa", "joista", "joita", "joka", "joka", "joksi", "jolla", "jolle", "jolta", "jona", "jonka", "jos", "jossa", "josta", "jota", "jotka", "kai", "kaikki", "kanssa", "kaukana", "keiden", "keihin", "keiksi", "keill�", "keille", "keilt�", "kein�", "keiss�", "keist�", "keit�", "keneen", "keneksi", "kenell�", "kenelle", "kenelt�", "kenen", "kenen�", "keness�", "kenest�", "kenet", "kenties", "keskell�", "kesken", "ket�", "ketk�", "ketk�", "koska", "koskaan", "kuin", "kuinka", "kuka", "kun", "kyll�", "l�hell�", "l�pi", "liian", "lla", "luona", "me", "meid�n", "meid�t", "meihin", "meill�", "meille", "meilt�", "meiss�", "meist�", "meit�", "mihin", "mik�", "miksi", "mill�", "mille", "milloin", "milloinkaan", "milt�", "min�", "mink�", "minua", "minulla", "minulle", "minulta", "minun", "minussa", "minusta", "minut", "minuun", "miss�", "mist�", "mit�", "miten", "mitk�", "mukaan", "mutta", "muut", "n�iden", "n�ihin", "n�iksi", "n�ill�", "n�ille", "n�ilt�", "n�in�", "n�iss�", "n�ist�", "n�it�", "n�m�", "ne", "niiden", "niihin", "niiksi", "niill�", "niille", "niilt�", "niin", "niin�", "niiss�", "niist�", "niit�", "noiden", "noihin", "noiksi", "noilla", "noille", "noilta", "noin", "noina", "noissa", "noista", "noita", "nopeasti", "nuo", "nyt", "oikea", "oikealla", "ole", "olemme", "olen", "olet", "olette", "oli", "olimme", "olin", "olisi", "olisimme", "olisin", "olisit", "olisitte", "olisivat", "olit", "olitte", "olivat", "olla", "olleet", "ollut", "on", "ovat", "paljon", "poikki", "puh", "saa", "saada", "se", "sek�", "sen", "siell�", "siihen", "siin�", "siit�", "siksi", "sill�", "sille", "silt�", "sin�", "sinua", "sinulla", "sinulle", "sinulta", "sinun", "sinussa", "sinusta", "sinut", "sinuun", "sit�", "ssa", "sta", "suoraan", "t�h�n", "tai", "takana", "takia", "t�ksi", "t�ll�", "t�lle", "t�lt�", "t�m�", "t�m�n", "t�n�", "t�ss�", "t�st�", "t�t�", "te", "teid�n", "teid�t", "teihin", "teill�", "teille", "teilt�", "teiss�", "teist�", "teit�", "tms", "tuo", "tuoda", "tuohon", "tuoksi", "tuolla", "tuolle", "tuolta", "tuon", "tuona", "tuossa", "tuosta", "tuota", "vaan", "v�h�n", "v�hemm�n", "vai", "vain", "vaikka", "vasen", "vasemmalla", "vastan", "viel�", "vieress�", "voi", "voida", "voit", "www", "yhdess�", "yli", "yl�s", "yms", "com", "fax", "klo", "my�s", "muuta", "viim", "asti", "sis", "koko", "alle", "joskus", "sivu", "paitsi", "sitten", "tule", "auki", "paras", "lue", "lis��", "joko", "ihan", "saat", "ei", "html") # finnish stopwords
removeSpecials <- function(x) gsub("[^0-9a-zA-Z������ ]", "", x) # function to remove special chars
k <- 4 # number of topics

# Ladataan metadata-excel

setwd(baseDir)
setwd("data/meta")

metadata_wb <- loadWorkbook("aineisto-metadata.xlsx")
metadata <- readWorksheet(metadata_wb, sheet = "Sheet1", header = T, colTypes = c("integer", "character", "character", "character", "character", "Date", "character", "character", "character", "character"))

# Set working dir to load data
setwd(baseDir)
setwd("data/docs")

# Remove empty documents so they don't cause problems later
a <- dir(getwd())
emptya <- a[file.info(a)[["size"]] == 0]
unlink(emptya)

#Get filenames
filenames <- list.files()

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
langlist <- list()
for (i in 1:length(docs)) { langlist[i] <- docs[[i]]$meta$language }
unilist <- unique(langlist)

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
setwd(baseDir)
setwd("results")

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
  
  # commented to avoid duplicate with Dokumentit ja todenn�k�isyydet.csv
  #write.csv(x.topicProbabilities, file = paste(h, "-", k, "topics -", t, "terms - Topic Probabilities.csv"))
  
  x.topic1ToTopic2 <- lapply(1:nrow(dtm), function(x) sort(x.topicProbabilities[x,])[k]/sort(x.topicProbabilities[x,])[k - 1])
  x.topic2ToTopic3 <- lapply(1:nrow(dtm), function(x) sort(x.topicProbabilities[x,])[k - 1]/sort(x.topicProbabilities[x,])[k - 2])
  write.csv(x.topic1ToTopic2, file = paste(h, "-", k, "topics -", t, "terms - Topic1 To Topic2.csv"))
  write.csv(x.topic2ToTopic3, file = paste(h, "-", k, "topics -", t, "terms - Topic2 To Topic3.csv"))
  
  # Luodaan dataframe, johon tallennetaan dokumentin nimi ja todenn�k�isyydet, tallennetaan se muuttujaan
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

# Yhdist� topic + probability data framet
topicprob <- rbind(finnish.corp_df, english.corp_df, swedish.corp_df)
write.csv(topicprob, file = "Kaikki kielet - Dokumentit ja todenn�k�isyydet.csv")

# Tallenna yksitt�iset topicprobit
write.csv(english.corp_df, file = "english.corp - Dokumentit ja todenn�k�isyydet (Topic Probabilities).csv")
write.csv(finnish.corp_df, file = "finnish.corp - Dokumentit ja todenn�k�isyydet (Topic Probabilities).csv")
write.csv(swedish.corp_df, file = "swedish.corp - Dokumentit ja todenn�k�isyydet (Topic Probabilities).csv")

# Yhdist� topicprob dokumenttien metadataan
full_metadata <- merge(x = metadata, y = topicprob, by.x = "tiedostonnimi.txt", by.y = "Dokumentti", all.x = T)

# Tallenna t�ysi metadata
library(xlsx) # for writing excel
write.xlsx(full_metadata, "Full Metadata (unclean).xlsx", showNA = F)
