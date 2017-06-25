## Text Analytics 

## Goal 
#  The goal of this project is to do sentiment analysis of the text which is to be scraped from the 
#  IMDB website and assigning them positive and negative sentiments 

## Setting up working directory and loading packages

install.packages("e1071")
install.packages("rvest")
library(rvest)
library(e1071)

## Scraping 

# The movie selected was Interstellar
# For scraping the reviews from IMDB, we use a for loop to scrape for 100 continuous pages.

# The 1000 reviews were first divided into half consisting of approx 500 positive reviews and 500 negative reviews.

# This one is Positive reviews scraping

counts = seq(0,500,10)
previews = NULL
pratings = NULL
ddr = NULL
for (j in counts){
  url1 = paste0("http://www.imdb.com/title/tt1375666/reviews?filter=love;filter=love;start=",j)
  #url2 = paste0("http://www.imdb.com/title/tt1375666/reviews?filter=hate;filter=hate;start=",j)
  
  page1 = read_html(url1)
  #page2 = read_html(url2)
  
  reviews1 = html_text(html_nodes(page1,'#tn15content p'))
  reviews.positive = setdiff(reviews1, c("*** This review may contain spoilers ***","Add another review"))
  #reviews.positive
  
  #reviews2 = html_text(html_nodes(page2,'#tn15content p'))
  #reviews.negative = setdiff(reviews2, c("*** This review may contain spoilers ***","Add another review"))
  
  
  
  movie.nodes.positive = html_nodes(page1,'h2 + img') 
  rr.positive  = html_attr(movie.nodes.positive,name='alt')
  
  #movie.nodes.negative = html_nodes(page2,'h2 + img')
  #rr.negative  = html_attr(movie.nodes.negative,name='alt')
  
  #ddr= c(ddr,rr.positive,rr.negative)
  #rat =substr(html_attr(movie.nodes.positive,name='alt'),0,2)
  #rat
  
  
  pratings = c(pratings,rr.positive)
  previews = c(previews,reviews.positive)
  
}

# make a dataframe out of the rating and reviews

pdata <- data.frame(previews, pratings)
pdata$previews <- as.character(pdata$previews)

# Now comes the Negative reviews scraping 

counts = seq(0,500,10)
nreviews = NULL
nratings = NULL
ddr = NULL
for (j in counts){
  #url1 = paste0("http://www.imdb.com/title/tt1375666/reviews?filter=love;filter=love;start=",j)
  url2 = paste0("http://www.imdb.com/title/tt1375666/reviews?filter=hate;filter=hate;start=",j)
  
  #page1 = read_html(url1)
  page2 = read_html(url2)
  
  #reviews1 = html_text(html_nodes(page1,'#tn15content p'))
  #reviews.positive = setdiff(reviews1, c("*** This review may contain spoilers ***","Add another review"))
  #reviews.positive
  
  reviews2 = html_text(html_nodes(page2,'#tn15content p'))
  reviews.negative = setdiff(reviews2, c("*** This review may contain spoilers ***","Add another review"))
  
  
  
  #movie.nodes.positive = html_nodes(page1,'h2 + img') 
  #rr.positive  = html_attr(movie.nodes.positive,name='alt')
  
  movie.nodes.negative = html_nodes(page2,'h2 + img')
  rr.negative  = html_attr(movie.nodes.negative,name='alt')
  
  #ddr= c(ddr,rr.positive,rr.negative)
  #rat =substr(html_attr(movie.nodes.positive,name='alt'),0,2)
  #rat
  
  
  nratings = c(nratings,rr.negative)
  nreviews = c(nreviews,reviews.negative)
  
}

ndata <- data.frame(nreviews, nratings)
ndata$nreviews <- as.character(ndata$nreviews)

## Next approach would be comparing these reviews with positive and negative lexicons which are derived by
## AFINN data. The following .TXT files conatin those positive and negative "Bag of Words". 

kPosTerms <- readLines("Positive.txt")
kNegTerms <- readLines("Negative.txt")

# get positive and negative wordlists

kPosText <- pdata$previews
kNegText <- ndata$nreviews

## Lets compute some sentiment scores we talked about

ComputeSentimentScores <- function(sentences){
  # ===========================================================================
  #     Compute sentiment score on a sentences dataframe
  #
  # Args:
  #   sentences: a dataframe of sentences
  #
  # Returns:
  #   scores: 
  # ===========================================================================
  
  scores <- lapply(sentences, ComputeSentimentScore)
  
  return(scores)
}


ComputeSentimentScore <- function(sentence, neg.terms=kNegTerms,
                                  pos.terms=kPosTerms) {
  # ===========================================================================
  #            Compute the sentiment score of a sentence
  #
  # Args:
  #   sentence: a string of words
  #   neg.terms: negative wordlist
  #   pos.terms: positive wordlist
  #
  # Returns:
  #   score (): (sentence, negative_matches, positive_matches)
  #             e.g. ("there will be happy and a sad day", "1", "2")
  # ===========================================================================
  
  # create holder for original sentence
  orig.sentence <- sentence
  
# remove unnecessary characters using chained substitutions
# TODO: look into TM package for better ways of doing this
  
  sentence <- tolower(gsub('\\d+', '', 
                           gsub('[[:cntrl:]]', '', 
                                gsub('[[:punct:]]', '', sentence))))
  
# split sentence into words
  words <- unlist(strsplit(sentence, '\\s+'))
  
  
 --------------------------------------------------------------------- 
# This step of removing the unnecesary characters is very crucial. 
# this can also be done using "tm" text mining package where a corpus is made and then various functions
# are applied. We will do that next time. 
 ---------------------------------------------------------------------    


# build vector with matches between words and each category
# and sum up the number of words in each category
  
  neg.matches <- sum(!is.na(match(words, neg.terms)))
  pos.matches <- sum(!is.na(match(words, pos.terms)))
  
  score <- c(orig.sentence, neg.matches, pos.matches)
  
  return(score)
}

# Build tables of positive and negative sentences with scores

pos.results <- t(rbind(data.frame(ComputeSentimentScores(kPosText))))
pos.results <- data.frame(pos.results, "Positive")

neg.results <- t(rbind(as.data.frame(ComputeSentimentScores(kNegText))))
neg.results <- data.frame(neg.results, "Negative")

# Rename the columns

colnames(pos.results) <- c('sentence', 'neg', 'pos', 'sentiment')
colnames(neg.results) <- c('sentence', 'neg', 'pos', 'sentiment')

total.results <- rbind(pos.results, neg.results)

# turn the outcome variable (last column) into a factor

total.results[,4] <- as.factor(total.results[,4])


# run the naive bayes model

NaiveBayesClassifier <- naiveBayes(total.results[,2:3], total.results[,4])

data <- total.results

prediction <- predict(NaiveBayesClassifier, data)

conf.matrix <- table(prediction, data[,4], dnn=list('predicted','actual'))
conf.matrix

# Accuracy check 
# we know the formula of accuracy, which can be derived form the confusion matrix having the 
# True positives and negatives and False positives and negatives.

#(379+409)/(379+409+131+101)
# 77.2 %





