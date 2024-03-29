# Install######################################################
#install.packages("tm")  # for text mining
#install.packages("SnowballC") # for text stemming
#install.packages("wordcloud") # word-cloud generator 
#install.packages("RColorBrewer") # color palettes
# Load######################################################
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
############# ######################################
text <- readLines("dataFiles/all_text.txt")
#text <- readLines(file.choose())
docs <- Corpus(VectorSource(text))
inspect(docs)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "Q:")
docs <- tm_map(docs, toSpace, "A:")
######################################################
inspect(docs)
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("itâs", "yeah", "get", "things", "see", "also", "lot","stuff", "thing", "much", "bit", "way", "take", "said", "always", "able","got", "okay", "put")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)


dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 20)
######################################################
######################################################
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=100, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Spectral"))













