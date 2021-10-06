
Sys.setenv("plotly_username"="linus.austen.mcmanamey")
Sys.setenv("plotly_api_key"="3tqgEzyZAGTIeFY5sQ1T")
####################################################################

####################################################################
generate_data <- function(id)
{
  statements <- read_excel("dataFiles/data.xlsx", sheet = "statements")
  nodes <- read_excel("dataFiles/data.xlsx", sheet = "nodes")
  node <- nodes[nodes$id == id,]
  statements <- statements[statements$id == id,]
  
  return(list("statement" = statements$text, "htmlString" = statements$htm, "group" = node$group, "id" = node$id, "label" = node$label, "title" = node$title, "value" = node$value,  "polarity" = node$polarity, "objectivity" = node$objectivity, "majorText" = node$majorText))
}
####################################################################
Rtools <- function()
{
  statements <- read_excel("dataFiles/data.xlsx", sheet = "statements")
  matrix <- create_matrix(cbind(statements["id"], statements["text"]),language="english", removeNumbers=TRUE, stemWords=FALSE, weighting=tm::weightTfIdf)
  container <- create_container(matrix, statements$type,trainSize=1:10, testSize=11:15, virgin=FALSE)
  models <- train_models(container, algorithms= "SVM")
  results <- classify_models(container, models)
  analytics <- create_analytics(container, results)
}
####################################################################
calScore <- function(x) 
{
  return(calculate_score(x))
}
########################################
calScore <- function(x) 
{
  return(calculate_score(x))
}
########################################
calSent <- function(x) 
{
  return(calculate_sentiment(x))
}
########################################
calPresence <- function(x) 
{
  return(calculate_total_presence_sentiment(x))
}
########################################
generate_wordCloud <- function(wcChoice)
{
  if (wcChoice == "Individual Transcripts")
  {
    text <- readLines("dataFiles/all_text.txt")
  }
  else if (wcChoice == "Launceston Fixed Session")
  {
    text <- readLines("dataFiles/launceston_transcript.txt")
  }
  else if (wcChoice == "Ulverstone Fixed Session")
  {
    text <- readLines("dataFiles/Ulverstone_transcript.txt")
  }
  docs <- Corpus(VectorSource(text))
  inspect(docs)
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "Q:")
  docs <- tm_map(docs, toSpace, "A:")
  docs <- tm_map(docs, toSpace, "Female:")
  ######################################################
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove english common stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  docs <- tm_map(docs, removeWords, c("\t", "\u0092s","don\u0092t ", "\u0092m", "\u0092ve", "\u0092re", "realli", "itâs","yeah", "get", "things", "see", "also", "lot","stuff", "thing", "much", "bit", "way", "take", "said", "always", "able","got", "okay", "put")) 
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Text stemming
  docs <- tm_map(docs, stemDocument)
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  return(list("dtm" = dtm, "m" = m, "v" = v, "d" = d))
}
######################################################

#statements <- read_excel("data.xlsx", sheet = "statements")

#scoreDF <- lapply(statements, calScore)
#sentDF <- lapply(statements, calSent)
#presDF <- lapply(statements, calPresence)
#statements['sentimentScore'] <- scoreDF$text
#statements['sentimentSent'] <- sentDF$sentiment


#nodes <- read_excel("data.xlsx", sheet = "nodes")
#node <- nodes[nodes$id == id,]
#statements <- statements[statements$id == id,]
#generate_data("26Ten_Program")


