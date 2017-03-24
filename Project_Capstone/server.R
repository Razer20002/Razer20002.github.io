library(tm)
library(RWeka)
library(tau)
library(stringr)
library(slam)
library(plyr)

data_3gramcsv <- read.csv("data_3gram.csv")
data_3gramcsv <- rename(data_3gramcsv,c(X="Name",X1="Freq"))

data_2gramcsv <- read.csv("data_2gram.csv")
data_2gramcsv <- rename(data_2gramcsv,c(X="Name",X1="Freq"))

data_1gramcsv <- read.csv("data_1gram.csv")
data_1gramcsv <- rename(data_1gramcsv,c(X="Name",X1="Freq"))

shinyServer(  
  function(input, output) {  
    output$greeting <- renderPrint({
      cat("I am thinking one of the following, the first entry is my guess.")
    })
    
    output$next_words <- renderPrint({ 
      
      ##Predict the next word
      
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

      Next_Word_Prediction(input$phrase)
      
    })
    
  }
)
