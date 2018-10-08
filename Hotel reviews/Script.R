library(magrittr)
library(tidytext)
library(dplyr)
library(dtplyr)
library(tm)
library(mlr)
library(parallelMap)

parallelStartSocket(2)

finn <- get_sentiments("afinn")
bing <- get_sentiments("bing")
nrc <- get_sentiments("nrc")
undesirable_words <- c("No positive", "No Negative", "AND", "it", "in")
stop_words <- stopwords(kind = "en")

# Regex pattern for removing stop words
stop_pattern <- paste0("\\b(", paste0(stopwords("en"), collapse = "|"), ")\\b")
undesirable_pattern <- paste0("\\b(", paste0(undesirable_words, collapse = "|"), ")\\b")

df <- read.csv("Hotel_Reviews.csv", stringsAsFactors = FALSE)

df$Combined_Review <- paste(df$Positive_Review, df$Negative_Review)
combined <- group_by(df, Hotel_Name)
combined$ID <- seq.int(nrow(combined))

pr <- as.data.frame(t(rbind(Review = combined$Positive_Review, Consensus = +1)))
nr <- as.data.frame(t(rbind(Review = combined$Negative_Review, Consensus = -1)))

all_reviews <- rbind(pr, nr)[1:40,]
all_reviews <- na.omit(all_reviews)
training_set <- all_reviews[1:20,]

#docs <- Corpus(VectorSource(combined$Positive_Review)) %>%
#    tm_map(removePunctuation) %>%
#    tm_map(removeNumbers) %>%
#    tm_map(cleanData)

task = makeClassifTask(data = training_set, target = "Consensus")
selected_model = makeLearner("classif.naiveBayes")
NB_mlr = train(selected_model, task)
NB_mlr$learner.model

predictions_mlr = as.data.frame(predict(NB_mlr, newdata = all_reviews))
table(predictions_mlr[, 1], all_reviews$Consensus)

parallelStop()