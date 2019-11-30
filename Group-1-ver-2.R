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

# ----------------------------------------------------------------------------------
# setting work directory
getwd()
setwd("C:/Users/azimz/Desktop/temp")
# import the dataset into R
review<-read.csv("HotelReviews.csv", sep = ",", stringsAsFactors = FALSE)
# summarize the data 
summary(review)
str(review)
colnames(review)
#-------------------------------------------------------------------------------
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
                     c("also", "get","like", "company", "made", "can", "im", "dress", "just", "i", "", "didnt",
                       "did", "do", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "just", "everything", 
                       "got", "get", "really", "stayed", "next"))



corpus_review[[8]][1]

# to remove any non-english text from reviews.text variable (column) 
rev_eng<-review[textcat(corpus_review)=="english",] 



# find 20 most frequent words 
word_freq<-freq_terms(corpus_review,20)
word_freq


# detect different languages in the review texts 
# textcat(corpus_review)


# Create the DTM & TDM from the corpus
# converting the corpus to a matrix 
review_dtm <- DocumentTermMatrix(corpus_review)
review_tdm <- TermDocumentMatrix(corpus_review)

# Convert TDM to matrix
review_m <- as.matrix(review_tdm)
# Sum rows and frequency data frame
review_term_freq <- rowSums(review_m)
# Sort term_frequency in descending order
review_term_freq <- sort(review_term_freq, decreasing = T)
# View the top 10 most common words
review_term_freq[1:10]


# Exploratory text analysis
plot(word_freq)
barplot(review_term_freq[1:20], col = "steel blue", las = 2)

# word cloud
# Create a wordcloud for the values in word_freqs
review_word_freq <- data.frame(term = names(review_term_freq),
                               num = review_term_freq)
wordcloud(review_word_freq$term, review_word_freq$num,
          max.words = 50, colors = "red")

# Print the word cloud with the specified colors
wordcloud(review_word_freq$term, review_word_freq$num,
          max.words = 50, colors = c("aquamarine","darkgoldenrod","tomato"))


# Simple word clustering
review_tdm2 <- removeSparseTerms(review_tdm, sparse = 0.9)
hc <- hclust(d = dist(review_tdm2, method = "euclidean"), method = "complete")
plot(hc)

# Word associations
# Create associations
associations <- findAssocs(review_tdm, "fit", 0.05)
# Create associations_df
associations_df <- list_vect2df(associations)[, 2:3]
# Plot the associations_df values 
ggplot(associations_df, aes(y = associations_df[, 1])) + 
  geom_point(aes(x = associations_df[, 2]), 
             data = associations_df, size = 3) + 
  ggtitle("Word Associations to 'fit'") + 
  theme_gdocs()



