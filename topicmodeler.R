

# Set up environment
# *****************

library(tm) # for text mining
library(topicmodels) # for LDA

setwd("C:/Users/mikkok/Downloads/csc-dataproject/data/documents-export-2016-06-17") # set working dir

# Create variables
suomiStops <- c("aina", "alla", "ehkä", "eivät", "emme", "en", "enemmän", "ennen", "et", "että", "ette", "hän", "häneen", "hänellä", "hänelle", "häneltä", "hänen", "hänessä", "hänestä", "hänet", "häntä", "he", "heidän", "heidät", "heihin", "heillä", "heille", "heiltä", "heissä", "heistä", "heitä", "hlö", "hlöä", "oikein", "http", "hyvin", "ilman", "itse", "ja", "jälkeen", "johon", "joiden", "joihin", "joiksi", "joilla", "joille", "joilta", "joina", "joissa", "joista", "joita", "joka", "joka", "joksi", "jolla", "jolle", "jolta", "jona", "jonka", "jos", "jossa", "josta", "jota", "jotka", "kai", "kaikki", "kanssa", "kaukana", "keiden", "keihin", "keiksi", "keillä", "keille", "keiltä", "keinä", "keissä", "keistä", "keitä", "keneen", "keneksi", "kenellä", "kenelle", "keneltä", "kenen", "kenenä", "kenessä", "kenestä", "kenet", "kenties", "keskellä", "kesken", "ketä", "ketkä", "ketkä", "koska", "koskaan", "kuin", "kuinka", "kuka", "kun", "kyllä", "lähellä", "läpi", "liian", "lla", "luona", "me", "meidän", "meidät", "meihin", "meillä", "meille", "meiltä", "meissä", "meistä", "meitä", "mihin", "mikä", "miksi", "millä", "mille", "milloin", "milloinkaan", "miltä", "minä", "minkä", "minua", "minulla", "minulle", "minulta", "minun", "minussa", "minusta", "minut", "minuun", "missä", "mistä", "mitä", "miten", "mitkä", "mukaan", "mutta", "muut", "näiden", "näihin", "näiksi", "näillä", "näille", "näiltä", "näinä", "näissä", "näistä", "näitä", "nämä", "ne", "niiden", "niihin", "niiksi", "niillä", "niille", "niiltä", "niin", "niinä", "niissä", "niistä", "niitä", "noiden", "noihin", "noiksi", "noilla", "noille", "noilta", "noin", "noina", "noissa", "noista", "noita", "nopeasti", "nuo", "nyt", "oikea", "oikealla", "ole", "olemme", "olen", "olet", "olette", "oli", "olimme", "olin", "olisi", "olisimme", "olisin", "olisit", "olisitte", "olisivat", "olit", "olitte", "olivat", "olla", "olleet", "ollut", "on", "ovat", "paljon", "poikki", "puh", "saa", "saada", "se", "sekä", "sen", "siellä", "siihen", "siinä", "siitä", "siksi", "sillä", "sille", "siltä", "sinä", "sinua", "sinulla", "sinulle", "sinulta", "sinun", "sinussa", "sinusta", "sinut", "sinuun", "sitä", "ssa", "sta", "suoraan", "tähän", "tai", "takana", "takia", "täksi", "tällä", "tälle", "tältä", "tämä", "tämän", "tänä", "tässä", "tästä", "tätä", "te", "teidän", "teidät", "teihin", "teillä", "teille", "teiltä", "teissä", "teistä", "teitä", "tms", "tuo", "tuoda", "tuohon", "tuoksi", "tuolla", "tuolle", "tuolta", "tuon", "tuona", "tuossa", "tuosta", "tuota", "vaan", "vähän", "vähemmän", "vai", "vain", "vaikka", "vasen", "vasemmalla", "vastan", "vielä", "vieressä", "voi", "voida", "voit", "www", "yhdessä", "yli", "ylös", "yms", "com", "fax", "klo", "myös", "muuta", "viim", "asti", "sis", "koko", "alle", "joskus", "sivu", "paitsi", "sitten", "tule", "auki", "paras", "lue", "lisää", "joko", "ihan", "saat", "ei", "html")
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE
k <- 5 # number of topics

# Load files
# **********

# Load files into Corpus
docs.vec <- VectorSource(DirSource(getwd()))
docs.corp <- Corpus(DirSource(getwd()))


# Preprocess data
# ***************

#for(j in seq(docs)){docs[[j]] <- gsub("[:punct:]", "", docs[[j]])}

docs.corp <- tm_map(docs.corp, tolower)
docs.corp <- tm_map(docs.corp, removePunctuation)
docs.corp <- tm_map(docs.corp, removeNumbers)
docs.corp <- tm_map(docs.corp, removeWords, stopwords("english"))
docs.corp <- tm_map(docs.corp, removeWords, suomiStops)
docs.corp <- tm_map(docs.corp, stripWhitespace)
docs.corp <- tm_map(docs.corp, PlainTextDocument)

tdm <- TermDocumentMatrix(docs.corp)

# Models topics
# *************

ldaOut <-LDA(tdm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))

# Write data out
# **************


--
  
  
  Mikko Koskela

mikkokoskela@kamk.fi

0407490575
