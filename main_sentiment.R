setwd("C:\\Users\\Jarvis\\Desktop\\Desk\\Sentiment Analysis")
Sys.setenv("JAVA_HOME"="C:\\Program Files\\Java\\jre1.8.0_77")
library(tidytext)
library(tm)
library(qdap)
library(ggplot2)
library(dplyr)
library(tidyr)

bos_reviews <- readRDS("bos_reviews.rds")

#bos_pol <- polarity(bos_reviews$comments)
bos_pol <- readRDS("bos_pol.rds")

# Summary for all reviews
summary(bos_pol$all$polarity)

# Plot it
ggplot(bos_pol$all, aes(x = polarity, y = ..density..)) +
  geom_histogram(binwdidth = 0.25, fill = "#bada55", colour = "grey60") +
  geom_density(size = 0.75)

# Review
bos_pol$group

# Add polarity column
bos_reviews_with_pol <- bos_reviews %>% 
  mutate(polarity = bos_pol$all$polarity)

# Subset positive comments 
pos_comments <- bos_reviews_with_pol %>% 
  filter(polarity>0) %>% 
  pull(comments)

# Subset negative comments
neg_comments <- bos_reviews_with_pol %>% 
  filter(polarity<0) %>% 
  pull(comments)

# Paste and collapse the positive comments
pos_terms <- paste(pos_comments, collapse = " ")

# Paste and collapse the negative comments
neg_terms <- paste(neg_comments, collapse = " ")

# Concatenate the terms
all_terms <- c(pos_terms, neg_terms)

# Pipe a VectorSource Corpus
all_corpus <- all_terms %>% 
  VectorSource() %>% 
  VCorpus()

# Simple TFIDF TDM
all_tdm <- TermDocumentMatrix(
  all_corpus, 
  control = list(
    weighting = weightTfIdf, 
    removePunctuation = TRUE, 
    stopwords = stopwords(kind = "en")
  )
)

# Examine the TDM
all_tdm

# Vector to tibble
tidy_reviews <- bos_reviews %>% 
  unnest_tokens(word, comments)

# Group by and mutate
tidy_reviews <- tidy_reviews %>% 
  group_by(id) %>% 
  mutate(original_word_order = seq_along(word))

#tidy_reviews

# Load stopwords
data("stop_words")

# Performing anti-join
tidy_reviews_without_stopwords <- tidy_reviews %>% 
  anti_join(stop_words)

bing <- get_sentiments("bing")

# Calculate polarity for each review
pos_neg <- tidy_reviews %>% 
  inner_join(bing) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>% 
  mutate(polarity = positive - negative)

# Check outcome
summary(pos_neg)

# Review tidy_reviews
tidy_reviews

# Review pos_neg
pos_neg

# Create effort
effort <- tidy_reviews %>%
  count(id)

# Inner join
pos_neg_with_effort <- inner_join(pos_neg,effort)

# Review 
pos_neg_with_effort

# Add pol
pos_neg_pol <- pos_neg_with_effort %>%
  mutate(
    pol = ifelse(
      polarity >= 0, 
      "Positive", 
      "Negative"
    )
  )

# Plot
ggplot(
  pos_neg_pol, 
  aes(polarity, n, color = pol)
) + 
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
   ggtitle("Relationship between word effort & polarity")

# Matrix
all_tdm_m <- as.matrix(all_tdm)

# Column names
colnames(all_tdm_m) <- c("positive","negative")

# Top pos words
order_by_pos <- order(all_tdm_m[, 1], decreasing = TRUE)

# Review top 10 pos words
all_tdm_m[order_by_pos, ] %>% head(10)

# Top neg words
order_by_neg <- order(all_tdm_m[,2], decreasing = TRUE)

# Review top 10 neg words
all_tdm_m[order_by_neg, ] %>% head(10)

# Comparison cloud
comparison.cloud(
  all_tdm_m, 
  max.words = 20,
  colors = c("darkgreen","darkred")
)

# Review
bos_pol$all[1:6,1:3]

# Scale/center & append
bos_reviews$scaled_polarity <- scale(bos_pol$all$polarity)

# Subset positive comments
pos_comments <- subset(bos_reviews$comments, bos_reviews$scaled_polarity>0)

# Subset negative comments
neg_comments <- subset(bos_reviews$comments, bos_reviews$scaled_polarity<0)

# Paste and collapse the positive comments
pos_terms <- paste(pos_comments, collapse = " ")

# Paste and collapse the negative comments
neg_terms <- paste(neg_comments, collapse = " ")

# Organize
all_terms<- c(pos_terms, neg_terms)

# VCorpus
all_corpus <- VCorpus(VectorSource(all_terms))

# TDM
all_tdm <- TermDocumentMatrix(
  all_corpus, 
  control = list(
    weighting = weightTfIdf, 
    removePunctuation = TRUE, 
    stopwords = stopwords(kind = "en")
  )
)

# Column names
all_tdm_m <- as.matrix(all_tdm)
colnames(all_tdm_m) <- c("positive", "negative")

# Comparison cloud
comparison.cloud(
  all_tdm_m, 
  max.words = 100,
  colors = c("darkgreen", "darkred")
)
