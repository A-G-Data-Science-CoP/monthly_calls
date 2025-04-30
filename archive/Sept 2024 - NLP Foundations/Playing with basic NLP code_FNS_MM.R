
# PREPARING DATA FROM FNS LEADERSHIP RETREAT --------------------------------------------------------------------------------

## Install and load necessary packages --------------------------------------------------------------------------------------

# If you've never used the package pacman before you'll need to install that with:
# install.packages("pacman")
# Then pacman's function p_load can install and load the rest, as below
pacman::p_load(tidyverse, readxl, tidytext, textFNS)

## Read in excel with the open ended question data --------------------------------------------------------------------------
folder <- "C:/Users/MadisonMerzke/A-G Associates/Data Analytics Resources - R/R Best Practices/Sept 2024 - NLP Foundations"

FNS <- read_excel(file.path(folder, "Example Data for Exercises_FNS Leadership Retreat Day Two Survey Results.xlsx")) %>%
  pivot_longer(1:3, names_to = "question", values_to = "answer") %>%
  filter(!is.na(answer))

## What other prep is needed for the types of NLP analyses we'll try? -------------------------------------------------------

# Looking at words alone
words <- FNS %>% 
  unnest_tokens(word, answer) %>%
  anti_join(stop_words)

# Then bigrams (two words together)
bigrams <- FNS %>%
  unnest_tokens(bigram, answer, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

# Getting rid of bigrams with stop words
bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  unite("bigram", word1:word2, sep = " ")

# Can also look at longer phrases
trigrams <- FNS %>%
  unnest_tokens(trigram, answer, token = "ngrams", n = 3) %>%
  filter(!is.na(trigram)) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  unite("trigram", word1:word3, sep = " ") 

# SOME BASIC ANALYSES -------------------------------------------------------------------------------------------------------

## Overall word frequency counts graphed ------------------------------------------------------------------------------------
words %>%
  count(word, sort = T) %>%
  filter(n > 5) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL) + 
  ggtitle("Word Frequency (Freq.>5)")

## Word frequency counts graphed by question --------------------------------------------------------------------------------
words %>%
  count(question, word, sort = T) %>%
  filter(n > 5) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  facet_wrap(~question, scales = "free_y") +
  geom_col() +
  labs(y = NULL) + 
  ggtitle("Word Frequency By Question (Freq.>5)") 

## Bigram frequency counts graphed by question -----------------------------------------------------------------------------
bigrams_filtered %>%
  count(question, bigram, sort = T) %>%
  filter(n > 1) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(n, bigram)) +
  facet_wrap(~question, scales = "free_y") +
  geom_col() +
  labs(y = NULL) + 
  ggtitle("Bigram Frequency By Question (Freq.>1)")

## 3-gram frequency counts graphed by question ----------------------------------------------------------------------------
trigrams %>%
  count(question, trigram, sort = T) %>%
  filter(n > 1) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(n, trigram)) +
  facet_wrap(~question, scales = "free_y") +
  geom_col() +
  labs(y = NULL) + 
  ggtitle("Trigram Frequency By Question (Freq.>1)")

## Negating bigram sentiment ----------------------------------------------------------------------------------------------
bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)
  
AFINN <- get_sentiments("afinn")

# not really a large enough FNS set for some of these
not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)

## Topic modeling -------------------------------------------------------------

section_words <- FNS %>%
  mutate(section = row_number()) %>%
  unnest_tokens(word, answer) %>%
  filter(!word %in% stop_words$word)

dtm <- section_words %>%
  count(section, word) %>%
  cast_dtm(section, word, n)

lda <- LDA(dtm, k = 2)

topics <- tidy(lda, matrix = "beta")

top_terms <- topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

beta_wide <- topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

docs <- tidy(lda, matrix = "gamma") %>%
  rename(section = document) %>%
  mutate(section = as.integer(section)) %>%
  left_join(FNS %>%
              mutate(section = row_number()))

topic1 <- docs %>%
  filter(topic==1, gamma>0.95)





