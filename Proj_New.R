# Load Text Mining package into current Workspace
library(tm)

cname <- file.path("~", "Downloads", "how-isis-uses-twitter")

docs <- Corpus(DirSource(cname))   

summary(docs)

inspect(docs)

docs <- tm_map(docs, removePunctuation)   

docs <- gsub("/", " ", docs)   

docs <- gsub("@", " ", docs)   

docs <- gsub("\\|", " ", docs) 

docs <- tm_map(docs, removeNumbers)


