#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
#@@@ ISIS twitter Data analysis @@@@#
#####         CIS - 660         #####
#####         Data Mining       #####
#####  Vishnuteja Thummanapelli #####
#####     Santhosh Tankala      #####
#####         Akhil Reddy       #####
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#

# Read CSV file into data

data <- read.csv(file = "~/Downloads/how-isis-uses-twitter/tweets.csv")

########################################### --- Part 1 ---- ###################################

#Load libraries 

library(tm) # tm -> Text Mining

## Convert the data to data frame (optional for our project)
tweets.df <- lapply(data, as.data.frame)

## Add the tweets columns to my_corpus

my_corpus <- Corpus(VectorSource(data$tweets)) #

# Check if there are any null values
is.null(my_corpus)

#Convert all tweets into lowercase
my_corpus <- tm_map(my_corpus, content_transformer(tolower))

typeof(my_corpus)

#Function for removing the Hyperlinks inside tweets
removeURL <- function(x)
  gsub("http[^[:space:]]*", "", x)

#Remove all url's from the tweets using above function
my_corpus <- tm_map(my_corpus, content_transformer(removeURL))

# Function to remove anything other than English letters or space
removeNumPunct <- function(x)
  gsub("[^[:alpha:][:space:]]*", "", x)

#Apply above function on my_corpus
my_corpus <- tm_map(my_corpus, content_transformer(removeNumPunct))

#Remove stopwords from tweets
#List of stopwords are obtained for english from the following link
#http://jmlr.csail.mit.edu/papers/volume5/lewis04a/a11-smart-stop-list/english.stop

myStopwords <-
  c(setdiff(
    stopwords('english'),
    c(
      "ENGLISH",
      "TRANSLATION",
      "TRANSCRIPT",
      "RT",
      "english",
      "translation",
      "transcript",
      "rt"
    )))

#apply above custom STOPWORDS function on my_corpus
my_corpus <- tm_map(my_corpus, removeWords, myStopwords)

# Remove extra whitespace
my_corpus <- tm_map(my_corpus, stripWhitespace)

# Keep a copy for stem completion later
my_corpusCopy <- my_corpus

# - Stemming the words
my_corpus <- tm_map(my_corpus, stemDocument)

#Apply Stem 
my_corpus <- Corpus(VectorSource(my_corpus))

#Print a sample tweet from the data after stemming
writeLines(strwrap(my_corpus[[190]]$content, 60))

#for exporting data after cleaning
dataframe <- data.frame(text = unlist(sapply(my_corpus, `[`, "content")), stringsAsFactors = F)

#Export Clean tweets into CSV. 
write.table(
  dataframe,
  file = "clean.csv",
  sep = ",",
  col.names = NA,
  qmethod = "double"
)

########################################### --- Part 2 (TF) ---- ###################################

#Find termDocument Matrix on Corpus
dt_matrix <- TermDocumentMatrix(my_corpus)

# List frequent terms with minimum frequency of 300
frequent_words <- findFreqTerms(dt_matrix,lowfreq=300)

# Print frequent terms
frequent_words

# Generate frequency matrix of all words
term_frequencies <- rowSums(as.matrix(dt_matrix))

# Print frequeny matrix of words
term_frequencies

# Generate subset of frequency matrix of words whose count or frequency is greater than 300. 
term_frequencies <- subset(term_frequencies, term_frequencies >= 300)

# Print the subset frequeny matrix of words
term_frequencies

# Generate data frame for Frequency matrix
data_frame <- data.frame(term = names(term_frequencies), freq = term_frequencies)

#Print data_frame
data_frame

# Load ggplot2 library for generating Words vs Frequency
library(ggplot2)

# ggplot of data_frame
ggplot(data_frame)

# Plot Words vs Frequency
ggplot(data_frame, aes(x = term, y = freq)) + geom_bar(stat = "identity") + coord_flip()


########################################### --- Part 3 (Word Cloud)---- ###################################

# Load wordcloud library

library(wordcloud)

term_frequencies <- rowSums(as.matrix(dt_matrix))

#wordcloud(names(term_frequencies), term_frequencies, min.freq = 200)

#wordcloud(names(term_frequencies), term_frequencies, max.words = 100)

# Generate word cloud with min frequency of 100

wordcloud(
  names(term_frequencies),
  term_frequencies,
  min.freq = 150,
  scale = c(5, .1),
  colors = brewer.pal(6, "Dark2")
)

######################################### --- Part 4 (Finding Associations)---- ####################################

# Find the words that are associated with word "USA"
usa <- findAssocs(dt_matrix,'usa',0.09)

usa

usa <- as.data.frame(usa)

names <- rownames(usa)

values <- usa$usa

usa_dataframe <- data.frame(term=names,value=values)

# Plot words 
ggplot(usa_dataframe, aes(x = names, y = values)) + geom_bar(stat = "identity") + xlab("Words") + ylab("'USA' word association with other words") + coord_flip()

## But the above results show that USA word is associated with Maldives ( Island in Indian ocean ). 
## Because ISIS is threatening people in Maldives for the recruitement. 
## Maldives is a good friend of USA ?

# Find the words that are associated with word "BOMB"
bomb <- findAssocs(dt_matrix,'bomb',0.09)

bomb <- as.data.frame(bomb)

names <- rownames(bomb)

values <- bomb$bomb

bomb_dataframe <- data.frame(term=names,value=values)

# Plot words 
ggplot(bomb_dataframe, aes(x = names, y = values)) + geom_bar(stat = "identity") + xlab("Words") + ylab("'BOMB' word association with other words") + coord_flip()

######################################### --- Part 5 (Clustering - hierarchical)---- ####################################

library(igraph)

# Remove the words 
new_dt_matrix <- removeSparseTerms(dt_matrix,0.98)

mtx_new <- as.matrix(new_dt_matrix)

distance_matrix <- dist(scale(mtx_new))

# Generate hierarchical clustering on words

fit <- hclust(distance_matrix, method = 'ward.D')

plot(fit)

######################################### --- Part 6 (Clustering - k means)---- ####################################

mtx_for_kmc <- t(new_dt_matrix)

k_value <-7

k_means <- kmeans(mtx_for_kmc, k_value)

typeof(mtx_for_kmc)

k_means

#Print centroids
k_means$centers

for(i in 1 : k_value)
  {
    cat(paste("Cluster No.", i ," -> ", sep=" "))
    s <- sort(k_means$centers[i,], decreasing=T)
    #k_means$centers
    cat(names(s)[1:10], "\n")
  }

######################################### --- Part 5 (Sentiment Analysis)---- ####################################

#Sentiment Analysis

library(devtools)

install_github("sentiment140", "okugami79")

library(sentiment)

sentiments <- sentiment(data$tweets)

table(sentiments$polarity)

require(data.table)

# sentiment plot
sentiments$score <- 0
sentiments$score[sentiments$polarity == "positive"] <- 1
sentiments$score[sentiments$polarity == "negative"] <- -1
sentiments$date <- as.IDate(data$time)
result <- aggregate(score ~ date, data = sentiments, sum)
plot(result, type = "l")

library(ggplot2)
library(readr)
library(dplyr)
library(reshape2)
library(tidyr)
library(formattable)
library(RColorBrewer)
library(lubridate)
library(networkD3)
library(plotly)
library(cluster)
library(viridis)
library(stringr)

data <- read_csv(file="~/Downloads/how-isis-uses-twitter/tweets.csv")

typeof(data)

data <- mutate(data, time=mdy_hm(time))
data <- mutate(data, hour=hour(time))
data <- mutate(data, wday=wday(time, label = TRUE))
data <- mutate(data, month=month(time))
data <- mutate(data, year=year(time))
data %>% arrange(time)
data <- mutate(data, yr_month=paste(year(time), month(time), sep='-'))
data <- mutate(data, date=ymd(paste(year(time), month(time), day(time), sep='-')))
data <- mutate(data, is_retweet = ifelse(grepl("^\\bRT\\b", tweets), "RT", "Original"))

data

set.seed(20)

## Find top hashtags

hash=data%>%
  mutate(hash=str_extract(tweets, "#\\w+"))%>%
  select(hash)%>%
  filter(!is.na(hash))%>%
  unnest(hash)%>%
  group_by(hash)%>%
  summarize(n_hash=n())%>%
  arrange(desc(n_hash))

#Print top hashtags

hash

# Find the time series trends of tweets

library(scales)

tweets=data%>%
  select(date,tweets)%>%
  arrange(date)%>%
  group_by(date)%>%
  summarize(n_tweet=n())%>%
  ggplot(aes(date,n_tweet))+
  geom_bar(fill="orange",size=.3, stat='identity',position='stack',color='black') +
  scale_x_date(
    
    limits = as.Date(c('2015-01-01','2016-08-01')))
ggplotly()

# Find hashtags trending at the time of peak time

hashspecial=data%>%
  filter(date=='2016-03-11'|date=='2016-03-27'|date=='2016-04-05')%>%
  mutate(hash=str_extract(tweets, "#\\w+"))%>%
  select(hash,date)

# Print hashtags

hashspecial$hash

# Create a dataset with username, no. of followers and no. of tweets from the actual data 

FANS = data%>%
  select(username,followers,tweets)%>%
  group_by(username)%>%
  summarize(followers=max(followers),tweets=n())

# K Means Culstering 

k_value <- 4

fans_cluster <- kmeans(as.matrix(FANS[, 2:3]), k_value)

# Plot clusters with the user names

ggplot(data = FANS, aes(tweets, followers))+
  geom_point(aes(color=paste("Cluster", factor(fans_cluster $ cluster))))+
  geom_text(aes(label=as.character(username)),show.legend = TRUE)+
  guides(color=FALSE)+
  ggtitle('Clusters based on No.of Tweets and No. of Followers')
ggplotly()

# Confused ? 
# So now we make it a little understandable with displaying the names whose followers > 5000 or Tweets > 500

ggplot(data = FANS, aes(tweets, followers))+
  geom_point(aes(color=paste("Cluster", factor(fans_cluster $ cluster))))+
  geom_text(aes(label=ifelse(followers>5000|tweets>500,as.character(username),'')),show.legend = TRUE)+
  guides(color=FALSE)+
  ggtitle('Clusters based on No.of Tweets and No. of Followers')
ggplotly()


mention=data%>%
  mutate(mention=str_extract(tweets, "@\\w+"))%>%
  select(mention)%>%
  filter(!is.na(mention))%>%
  unnest(mention)%>%
  group_by(mention)%>%
  summarize(n_mention=n())%>%
  arrange(desc(n_mention))
mention%>%
  head(10)


#Most active weekday and hour Due to sparse twitter activity in 2015, we zoom in to 2016

hourly=data%>%
  filter(year==2016)%>%
  select(hour,wday,yr_month,is_retweet,tweets)%>%
  group_by(hour,wday,yr_month,is_retweet)%>%
  summarize(n_tweet=n())%>%
  glimpse()
