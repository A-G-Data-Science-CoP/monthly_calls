
# PREPARING DATA FROM SAMHSA PUBLIC COMMENTS --------------------------------------------------------------------------------

# Note: much of the code and techniques used come from: 
# https://www.tidytextmining.com/

## Install and load necessary packages --------------------------------------------------------------------------------------

# If you've never used the package pacman before you'll need to install that with:
# install.packages("pacman")
# Then pacman's function p_load can install and load the rest, as below
pacman::p_load(tidyverse, readxl, tidytext, textdata, igraph, ggraph, widyr, topicmodels)

## Read in excel with the open ended data --------------------------------------------------------------------------
# If we want a larger data set for some of these, could we use the old SAMSHA comments?
folder <- "C:/Users/MadisonMerzke/OneDrive - A-G Associates/42 CFR Part 8 NPRM Comment Analysis"

SAMHSA <- read_excel(file.path(folder, "SAMHSA Comments_Edited.xlsx"), sheet = 4, skip = 1) %>%
  select(2) %>%
  rename(comment = `Comment (Blank for Attachment Files)`) %>%
  filter(!is.na(comment))

## What other prep is needed for the types of NLP analyses we'll try? -------------------------------------------------------

# Looking at words alone
words <- SAMHSA %>% 
  unnest_tokens(word, comment) %>%
  anti_join(stop_words)

# Then bigrams (two words together)
bigrams <- SAMHSA %>%
  unnest_tokens(bigram, comment, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

# Getting rid of bigrams with stop words
bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  unite("bigram", word1:word2, sep = " ")

# Can also look at longer phrases
trigrams <- SAMHSA %>%
  unnest_tokens(trigram, comment, token = "ngrams", n = 3) %>%
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
  filter(n > 120) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL) + 
  ggtitle("Word Frequency (Freq.>100)")

## Bigram frequency counts graphed -----------------------------------------------------------------------------
bigrams_filtered %>%
  count(bigram, sort = T) %>%
  filter(n > 35) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(n, bigram)) +
  geom_col() +
  labs(y = NULL) + 
  ggtitle("Bigram Frequency (Freq.>35)")

## 3-gram frequency counts graphed ----------------------------------------------------------------------------
trigrams %>%
  count(trigram, sort = T) %>%
  filter(n > 20) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(n, trigram)) +
  geom_col() +
  labs(y = NULL) + 
  ggtitle("Trigram Frequency (Freq.>20)")

# rm(list = setdiff(ls(), "SAMHSA"))

## Negating bigram sentiment ----------------------------------------------------------------------------------------------
bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)
  
AFINN <- get_sentiments("afinn")

not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)

not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"")

negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE)

negated_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"") +
  facet_wrap(~word1, nrow = 2, scales = "free_y")

## Visualizing a network of bigrams --------------------------------------------

bigram_graph <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = T) %>%
  filter(n > 30) %>%
  graph_from_data_frame()

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# rm(list = setdiff(ls(), "SAMHSA"))

## Counting and correlating among comments -----------------------------

section_words <- SAMHSA %>%
  mutate(section = row_number()) %>%
  unnest_tokens(word, comment) %>%
  filter(!word %in% stop_words$word)

word_pairs <- section_words %>%
  pairwise_count(word, section, sort = TRUE)

word_cors <- section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors %>%
  filter(item1 %in% c("surveys", "requirements", "standards", "verification")) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

word_cors %>%
  filter(correlation > .95) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

rm(list = setdiff(ls(), c("section_words", "SAMHSA")))

## Topic modeling -------------------------------------------------------------

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





