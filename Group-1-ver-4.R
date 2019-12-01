#-----------------------------------------------------Required packages-----------------------------------------
# install.packages("ggthemes")
# install.packages("qdap")
# install.packages("dplyr")
# install.packages("tm")
# install.packages("wordcloud")
# install.packages("plotrix")
# install.packages("dendextend")
# install.packages("ggplot2")
# install.packages("ggthemes")
# install.packages("RWeka")
# install.packages("reshape2")
# install.packages("quanteda")
# install.packages("textcat")
library(qdap)
library(dplyr)
library(tm)
library(wordcloud)
library(plotrix)
library(dendextend)
library(ggplot2)
library(ggthemes)
library(RWeka)
library(reshape2)
library(quanteda)
library(stringi)
library(textcat)
memory.limit(size=4000000)
# ----------------------------------------------------------------------------------
# setting work directory
getwd()
setwd("C:/Users/3735697/Desktop/Data 630/")
# import the dataset into R
review<-read.csv("HotelReviews.csv", sep = ",", stringsAsFactors = FALSE)
# summarize the data 
summary(review)
str(review)
colnames(review)

review$Sentiment[review$reviews.rating<3]<-"Negative"
review$Sentiment[review$reviews.rating==3]<-"Neutral"
review$Sentiment[review$reviews.rating>3]<-"Positive"

negativereview <- subset(review, review$Sentiment=="Negative")
#-------------------------------------------------------------------------------
# Convert the negativereview text into a collection of text documents
corpus_negativereview<-Corpus(VectorSource(negativereview$reviews.text))

# Text extraction
# Convert to lower case
corpus_negativereview<-tm_map(corpus_negativereview, tolower)

# to remove punctuation
corpus_negativereview<-tm_map(corpus_negativereview, removePunctuation)

# remove stopwords in english 
corpus_negativereview=tm_map(corpus_negativereview, removeWords, stopwords("english"))
corpus_negativereview=tm_map(corpus_negativereview, removeWords, stopwords("dutch"))

# manual stop word list
# Remove context specific stop words
corpus_negativereview=tm_map(corpus_negativereview, removeWords,
                             c("place","stay", "room", "good","hotel", "also","like", "company", "made", "can", "im", "dress", "just", "i", "", "didnt",
                               "did", "do", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "just", "everything", 
                               "got", "get", "really", "stayed", "next",'the','be','and','of','a','in','to','have','it','I','that','for','you','he','with','on','do','say','this','they','at','but','we','his','from','not','by','she','or','as','what','go','their','can','who','get','if','would','her','all','my','make','about','know','will','up','one','time','there','year','so','think','when','which','them','some','me','people','take','out','into','just','see','him','your','come','could','now','than','like','other','how','then','its','our','two','more','these','want','way','look','first','new','because','day','use','no','man','find','here','thing','give','many','well','only','those','tell','very','even', 'us' 
                             ))


## Stem document
corpus_negativereview=tm_map(corpus_negativereview, stemDocument)
corpus_negativereview[[8]][1]

# to remove any non-english text from negativereviews.text variable (column) 
rev_eng<-negativereview[textcat(corpus_negativereview)=="english",] 



# find 20 most frequent words 
word_freq<-freq_terms(corpus_negativereview,20)
word_freq


# detect different languages in the negativereview texts 
# textcat(corpus_negativereview)


# Create the DTM & TDM from the corpus
# converting the corpus to a matrix 
negativereview_dtm <- DocumentTermMatrix(corpus_negativereview)
negativereview_tdm <- TermDocumentMatrix(corpus_negativereview)
gc()
# Convert TDM to matrix
negativereview_m <- as.matrix(negativereview_tdm)
gc()
# Sum rows and frequency data frame
negativereview_term_freq <- rowSums(negativereview_m)
# Sort term_frequency in descending order
negativereview_term_freq <- sort(negativereview_term_freq, decreasing = T)
# View the top 10 most common words
negativereview_term_freq[1:10]


# Exploratory text analysis
plot(word_freq)
barplot(negativereview_term_freq[1:20], col = "red", las = 2)

# word cloud
# Create a wordcloud for the values in word_freqs
negativereview_word_freq <- data.frame(term = names(negativereview_term_freq),
                                       num = negativereview_term_freq)
wordcloud(negativereview_word_freq$term, negativereview_word_freq$num,
          max.words = 50, colors = "red")

# Print the word cloud with the specified colors
wordcloud(negativereview_word_freq$term, negativereview_word_freq$num,
          max.words = 50, colors = c("aquamarine","darkgoldenrod","tomato"))


# Simple word clustering
negativereview_tdm2 <- removeSparseTerms(negativereview_tdm, sparse = 0.9)
hc <- hclust(d = dist(negativereview_tdm2, method = "euclidean"), method = "complete")
plot(hc)

# Word associations
# Create associations
associations <- findAssocs(negativereview_tdm, "fit", 0.05)
# Create associations_df
associations_df <- list_vect2df(associations)[, 2:3]
# Plot the associations_df values 
ggplot(associations_df, aes(y = associations_df[, 1])) + 
  geom_point(aes(x = associations_df[, 2]), 
             data = associations_df, size = 3) + 
  ggtitle("Word Associations to 'fit'") + 
  theme_gdocs()

#-------------------------------------------------------------------------------

positivereview <- subset(review, review$Sentiment=="Positive")
# Convert the positivereview text into a collection of text documents
corpus_positivereview<-Corpus(VectorSource(positivereview$reviews.text))

# Text extraction
# Convert to lower case
corpus_positivereview<-tm_map(corpus_positivereview, tolower)

# to remove punctuation
corpus_positivereview<-tm_map(corpus_positivereview, removePunctuation)

# remove stopwords in english 
corpus_positivereview=tm_map(corpus_positivereview, removeWords, stopwords("english"))
corpus_positivereview=tm_map(corpus_positivereview, removeWords, stopwords("dutch"))

# manual stop word list
# Remove context specific stop words
corpus_positivereview=tm_map(corpus_positivereview, removeWords,
                             c("great","room","place","stay", "good", "nice", "room","hotel", "also","like", "company", "made", "can", "im", "dress", "just", "i", "", "didnt",
                               "did", "do", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "just", "everything", 
                               "got", "get", "really", "stayed", "next",'the','be','and','of','a','in','to','have','it','I','that','for','you','he','with','on','do','say','this','they','at','but','we','his','from','not','by','she','or','as','what','go','their','can','who','get','if','would','her','all','my','make','about','know','will','up','one','time','there','year','so','think','when','which','them','some','me','people','take','out','into','just','see','him','your','come','could','now','than','like','other','how','then','its','our','two','more','these','want','way','look','first','new','because','day','use','no','man','find','here','thing','give','many','well','only','those','tell','very','even', 'us' 
                             ))


## Stem document
corpus_positivereview=tm_map(corpus_positivereview, stemDocument)
corpus_positivereview[[8]][1]

# to remove any non-english text from positivereviews.text variable (column) 
rev_eng<-positivereview[textcat(corpus_positivereview)=="english",] 



# find 20 most frequent words 
word_freq<-freq_terms(corpus_positivereview,20)
word_freq


# detect different languages in the positivereview texts 
# textcat(corpus_positivereview)


# Create the DTM & TDM from the corpus
# converting the corpus to a matrix 
positivereview_dtm <- DocumentTermMatrix(corpus_positivereview)
positivereview_tdm <- TermDocumentMatrix(corpus_positivereview)
gc()
# Convert TDM to matrix
positivereview_m <- as.matrix(positivereview_tdm)
gc()
# Sum rows and frequency data frame
positivereview_term_freq <- rowSums(positivereview_m)
# Sort term_frequency in descending order
positivereview_term_freq <- sort(positivereview_term_freq, decreasing = T)
# View the top 10 most common words
positivereview_term_freq[1:10]


# Exploratory text analysis
plot(word_freq)
barplot(positivereview_term_freq[1:20], col = "green", las = 2)

# word cloud
# Create a wordcloud for the values in word_freqs
positivereview_word_freq <- data.frame(term = names(positivereview_term_freq),
                                       num = positivereview_term_freq)
wordcloud(positivereview_word_freq$term, positivereview_word_freq$num,
          max.words = 50, colors = "red")

# Print the word cloud with the specified colors
wordcloud(positivereview_word_freq$term, positivereview_word_freq$num,
          max.words = 50, colors = c("aquamarine","darkgoldenrod","tomato"))


# Simple word clustering
positivereview_tdm2 <- removeSparseTerms(positivereview_tdm, sparse = 0.9)
hc <- hclust(d = dist(positivereview_tdm2, method = "euclidean"), method = "complete")
plot(hc)

# Word associations
# Create associations
associations <- findAssocs(positivereview_tdm, "fit", 0.05)
# Create associations_df
associations_df <- list_vect2df(associations)[, 2:3]
# Plot the associations_df values 
ggplot(associations_df, aes(y = associations_df[, 1])) + 
  geom_point(aes(x = associations_df[, 2]), 
             data = associations_df, size = 3) + 
  ggtitle("Word Associations to 'fit'") + 
  theme_gdocs()


poop <- rbind(negativereview_m, positivereview_m[, colnames(negativereview_m)])
#------------------------------------------------------------------------------------------
memory.limit(size=4000000)
# Convert the review text into a collection of text documents
corpus_review<-Corpus(VectorSource(review$reviews.text))

# Text extraction
# Convert to lower case
corpus_review<-tm_map(corpus_review, tolower)

# to remove punctuation
corpus_review<-tm_map(corpus_review, removePunctuation)

# remove stopwords in english 
corpus_review=tm_map(corpus_review, removeWords, stopwords("english"))
corpus_review=tm_map(corpus_review, removeWords, stopwords("dutch"))

# manual stop word list
# Remove context specific stop words
corpus_review=tm_map(corpus_review, removeWords,
                     c("place","room","hotel", "also","like", "company", "made", "can", "im", "dress", "just", "i", "", "didnt",
                       "did", "do", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "just", "everything", 
                       "got", "get", "really", "stayed", "next",'the','be','and','of','a','in','to','have','it','I','that','for','you','he','with','on','do','say','this','they','at','but','we','his','from','not','by','she','or','as','what','go','their','can','who','get','if','would','her','all','my','make','about','know','will','up','one','time','there','year','so','think','when','which','them','some','me','people','take','out','into','just','see','him','your','come','could','now','than','like','other','how','then','its','our','two','more','these','want','way','look','first','new','because','day','use','no','man','find','here','thing','give','many','well','only','those','tell','very','even', 'us' 
                     ))

corpus_review=tm_map(corpus_review, stemDocument)
# to remove any non-english text from reviews.text variable (column) 
review_dtm <- DocumentTermMatrix(corpus_review)
review_tdm <- TermDocumentMatrix(corpus_review)
review_m <- as.matrix(review_tdm)
colnames(review_m)<-c("Positive","Negative")

review_term_freq <- rowSums(review_m)
review_word_freq <- data.frame(term=names(review_term_freq), num = review_term_freq)

commonality.cloud(review_m, 
                  colors = "steelblue1",
                  max.words = 50)
# Create comparison cloud
comparison.cloud(review_m,
                 colors = c("green", "red"),
                 max.words = 50)