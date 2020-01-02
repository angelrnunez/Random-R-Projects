# library required for assignment 
library(stringr) 
library(tidyverse)

#####################################################################################################
# FUNCTION USED TO CLEAN A SINGLE LINE OF TEXT 
#####################################################################################################
Clean_String <- function(string) 
{
  # changes uppercase to lowercase 
  temp <- tolower(string) 
  
  # deletes anything that isn't a number or letter 
  temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ") 
  
  # reduces to one white space
  temp <- stringr::str_replace_all(temp,"[\\s]+"," ") 
  
  # splits into single words
  temp <- stringr::str_split(temp, " ")[[1]]
  
  # eliminates trailing ""
  indexes <- which(temp == "")
  if(length(indexes) > 0) 
    {
    temp <- temp[-indexes]
  } 
  
  # returns single line of clean text 
  return(temp)
} 

#####################################################################################################
# FUNCTION USED TO CLEAN WHOLE TEXT 
#####################################################################################################
Clean_Text_Block <- function(text)
{
  # gets rid of blank lines in an if statement
  indexes <- which(text == "") 
  if (length(indexes) > 0) 
    {
      temp <- text[-indexes]
    }

  # returns zeros if there is no text 
  if (length(text) == 0) 
    {
      cat("There was no text in this document! \n")
      to_return <- list(num_tokens = 0, unique_tokens = 0, text = "")
    } 

  else 
    {
      # initalizes clean_text 
      clean_text <- NULL 
        # adds single lines of clean text together
        for (i in 1:length(text)) 
          {
            # uses function to clean lines and concatenate them together
            clean_text <- c(clean_text, Clean_String(text[i])) 
          }
    
    # clean text length 
    clean_text_length <- length(clean_text) 
    
    # initalizes word frequency to zero 
    zero <- rep.int(0,clean_text_length) 
    word_freq <- zero
    
    # calculates word frequencies using a for loop
    for (i in 1:clean_text_length) 
      {
        word_freq[i] <- sum(clean_text == clean_text[i])
      }
    }
  
  # creates a data frame with word and it's frequency 
  table <- data.frame(
      word = clean_text, 
      frequency = word_freq
    ) 
  
  # keeps only distinct words in table, eliminates repeats
  table <- distinct(table, word, .keep_all = TRUE)
  
  # orders table based on word gfrequency 
  table <- table[with(table, order(-frequency)), ]
  
  # creates vector with rank values 
  rank <- seq(1:6476)
  
  # creates new data frame with word, frequency, and it's rank 
  table <- data.frame(
      word = table$word, 
      frequency = table$frequency, 
      rank = rank 
    ) 
    
  # returns final data frame table for text 
  to_return <- table
  return(to_return)

}

#####################################################################################################
# READS IN TEXT AND OUTPUTS RESULTS & GRAPHS
#####################################################################################################
# reads in text file
input <- file("http://www.gutenberg.org/cache/epub/16726/pg16726.txt","r", blocking = FALSE )
text <- readLines(input)
close(input) 

# cleans text through function 
results <- Clean_Text_Block(text)
write.table(results,"ZipfResults.txt",sep="\t",row.names=FALSE)

# plots frequency versus rank 
frequency_words <- as.integer(results$frequency) 
rank_words <- results$rank
plot(frequency_words, rank_words, main = 'Frequency vs Rank', xlab = 'Word Frequency', ylab = 'Word Rank')

# plots ln(frequency) versus ln(rank) 
frequency_words <- log(as.integer(results$frequency) )
rank_words <- log(results$rank)
plot(frequency_words, rank_words, main = 'Log(Frequency) vs Log(Rank)', 
     xlab = 'Word Log(Frequency)', ylab = 'Word Log(Rank)')

# creates a linear fir for log(frequency) and lof(rank) 
lm <- lm(log(as.integer(results$frequency))~log(results$rank)) 

# plots linear regression line and graph for log(frequency) and lof(rank) 
plot(frequency_words, rank_words, main = 'Log(Frequency) vs Log(Rank)', 
     xlab = 'Word Log(Frequency)', ylab = 'Word Log(Rank)')
abline(lm)