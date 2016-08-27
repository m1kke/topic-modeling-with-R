# Load libraries
library(tm)
library(topicmodels)

# Create variables
dataDir <- "C:/Users/Mikko/Data Analysis Projects/topic-modeling-with-R/data"

# Set working dir
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
removeSpecials <- function(x) gsub(""."", "",x)
docs <- tm_map(docs, removeSpecials)

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
rownames(dtm) <- filenames

# Model topics
ldaOut <-LDA(dtm, k = 3)
