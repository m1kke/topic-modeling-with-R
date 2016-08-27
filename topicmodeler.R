### Set up environment

library(tm) # for text mining
library(topicmodels) # for LDA

# Set working dir
setwd("C:/Users/Mikko/Data Analysis Projects/topic-modeling-with-R/data")

# Load files into Corpus
docs <- Corpus(DirSource(getwd()))

# Convert to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove punctuation
docs <- tm_map(docs, removePunctuation)

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

# Strip whitespace
docs <- tm_map(docs, stripWhitespace)

# Turn into plaintext document
docs <- tm_map(docs, PlainTextDocument)

# Create DTM

dtm <- DocumentTermMatrix(docs)
rownames(dtm) <- c("test.txt")

# Model topics

ldaOut <-LDA(dtm, k = 3)

ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))

# Write data out
# **************


--
  
  
  Mikko Koskela

mikkokoskela@kamk.fi

0407490575
