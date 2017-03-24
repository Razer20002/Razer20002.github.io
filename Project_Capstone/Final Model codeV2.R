#Create dataset for analysis

library(tm)
library(RWeka)
library(tau)
library(stringr)
library(slam)
library(plyr)

twitter_data <- readLines("en_US.twitter.txt", n = 30000, encoding = "UTF-8", skipNul = TRUE)
news_data <- readLines("en_US.news.txt", n = 30000, encoding = "UTF-8", skipNul = TRUE)
blogs_data <- readLines("en_US.blogs.txt", n = 30000, encoding = "UTF-8", skipNul = TRUE)

all_data <- c(twitter_data, news_data,blogs_data)
all_data <- iconv(all_data, "latin1", "ASCII", sub="")

remove(twitter_data, news_data, blogs_data)

#Create corpus
Vsource <- VectorSource(all_data)
remove(all_data)
data_corpus <- Corpus(Vsource)

#clean the corpus
data_corpus <- tm_map(data_corpus, content_transformer(tolower))
data_corpus <- tm_map(data_corpus, removePunctuation)
data_corpus <- tm_map(data_corpus, stripWhitespace)
data_corpus <- tm_map(data_corpus, removeNumbers)
data_corpus <- tm_map(data_corpus, PlainTextDocument)

#Build twitter data model
# Trigrams
unigramTokenizer <- function(x) NGramTokenizer(x, 
                                               Weka_control(min = 1, max = 1))

bigramTokenizer <- function(x) NGramTokenizer(x, 
                                              Weka_control(min = 2, max = 2))

trigramTokenizer <- function(x) NGramTokenizer(x, 
                                               Weka_control(min = 3, max = 3))

data_1gram <- TermDocumentMatrix(data_corpus, control = list(tokenize = unigramTokenizer))

data_2gram <- TermDocumentMatrix(data_corpus, control = list(tokenize = bigramTokenizer))

data_3gram <- TermDocumentMatrix(data_corpus, control = list(tokenize = trigramTokenizer))
remove(data_corpus, Vsource)


#Output as csv for upload

#3gram

tdm_rollup <- rollup(data_3gram, 2, na.rm = TRUE, FUN = sum)
filter_matrix <- as.matrix(tdm_rollup)

write.csv(filter_matrix, "data_3gram.csv")

#2gram
tdm_rollup <- rollup(data_2gram, 2, na.rm = TRUE, FUN = sum)
filter_matrix <- as.matrix(tdm_rollup)

write.csv(filter_matrix, "data_2gram.csv")

#1gram
tdm_rollup <- rollup(data_1gram, 2, na.rm = TRUE, FUN = sum)
filter_matrix <- as.matrix(tdm_rollup)

write.csv(filter_matrix, "data_1gram.csv")

remove (filter_matrix, tdm_rollup)

remove(data_1gram,data_2gram,data_3gram)

##Predict the next word

data_3gramcsv <- read.csv("data_3gram.csv")
data_3gramcsv <- rename(data_3gramcsv,c(X="Name",X1="Freq"))

data_2gramcsv <- read.csv("data_2gram.csv")
data_2gramcsv <- rename(data_2gramcsv,c(X="Name",X1="Freq"))

data_1gramcsv <- read.csv("data_1gram.csv")
data_1gramcsv <- rename(data_1gramcsv,c(X="Name",X1="Freq"))


#First we must clean the string to compare with our ngrams

clean_string <- function(input_string) {
  testsource <- VectorSource(input_string)
  test_corpus <- Corpus(testsource)
  
  #clean the corpus
  test_corpus <- tm_map(test_corpus, content_transformer(tolower))
  test_corpus <- tm_map(test_corpus, removePunctuation)
  test_corpus <- tm_map(test_corpus, stripWhitespace)
  test_corpus <- tm_map(test_corpus, removeNumbers)
  test_corpus <- tm_map(test_corpus, PlainTextDocument)
  
  test_frame <- data.frame(text=unlist(sapply(test_corpus, `[`, "content")), stringsAsFactors=F)
  return_vector <- as.vector(test_frame[[1]])
  return(return_vector)
}


#Define Next_word function
Next_Word_ngram <- function(input_phrase,data_ngrams)
{
  #tdm_filter <- grep(input_phrase, data_ngrams$dimnames$Terms)
  #tdm_rollup <- rollup(data_ngrams[tdm_filter,], 2, na.rm = TRUE, FUN = sum)
  #filter_matrix <- as.matrix(tdm_rollup)
  prediction <- vector(length = 5)
  
  #x <- rowSums(filter_matrix)
  
  x <- data_ngrams[grep(input_phrase,data_ngrams$Name),]
  
  if(length(x) == 0) {
    return(prediction)
  } else {
    x2 <- x[order(-x$Freq, x$Name),]
    x3 <- as.vector(x2$Name[1:5])
    for(i in 1:5)
    {
      prediction[i]<- tail(strsplit(x3[i],split=" ")[[1]],1)
    }
    return(prediction)
  }
}

#Next we filter through our ngram tables to come up with a prediction
Next_Word_Prediction <- function(input_string)
{
  #Clean input string
  cleaned_string <- clean_string(input_string)
  
  #split words for ngrams
  phrase_temp <- tail(strsplit(cleaned_string,split=" ")[[1]],2)
  phrase_length <- length(phrase_temp)
  if(phrase_length ==2)
  {
    phrase2 <- paste("^",paste(phrase_temp[1], phrase_temp[2], sep = " ")," ", sep = "")
    phrase1 <- paste("^",phrase_temp[2]," ", sep = "")
  } else {
    phrase1 <- paste("^",phrase_temp[1]," ", sep = "")
  }
  
  
  if(phrase_length == 2) {
    #Check3grams
    result_ngram <- Next_Word_ngram(phrase2,data_3gramcsv)
  } else {
    result_ngram <- vector()
  }
  
  if(phrase_length >= 1) {
    if(is.na(result_ngram[1]) == TRUE) {
      #Check2grams
      result_ngram <- Next_Word_ngram(phrase1,data_2gramcsv)
    } 
    
    if(is.na(result_ngram[1]) == TRUE) {
      #checkUnigram
      result_ngram <- Next_Word_ngram(phrase1,data_1gramcsv)
    } 
  }
  if(is.na(result_ngram[1]) == TRUE) {
    #Get most common words
    result_ngram <- Next_Word_ngram("",data_1gramcsv)
  }
  prediction_return <- result_ngram[!is.na(result_ngram)]
  
  return(prediction_return)
}
test <- Next_Word_Prediction(c("My name is Jason, I am 28 and I like to"))
test





