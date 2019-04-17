library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(ggplot2)
library(stringr)
library(tidytext)
library(widyr)
library(ggraph)
library(igraph)
library(topicmodels)


set.seed(2019)

training_folder <- "20news-bydate/20news-bydate-train/"

# Define a function to read all files from a folder into a data frame
read_folder <- function(infolder) {
  tibble(file = dir(infolder, full.names = TRUE)) %>%
    mutate(text = map(file, read_lines)) %>%
    transmute(id = basename(file), text) %>%
    unnest(text)
}

# Use unnest() and map() to apply read_folder to each subfolder
raw_text <- tibble(folder = dir(training_folder, full.names = TRUE)) %>%
  unnest(map(folder, read_folder)) %>%
  transmute(newsgroup = basename(folder), id, text)

raw_text

# lets see what our is 
raw_text %>%
  group_by(newsgroup) %>%
  summarize(messages = n_distinct(id)) %>%
  ggplot(aes(newsgroup, messages)) +
  geom_col() +
  coord_flip()

# we can see that Usenet newsgroup names are named hierarchically
# starting with a main topic such as “talk”, “sci”, or “rec”


### Pre- Processing
# Removing every message has a header, containing field such as “from:” or “in_reply_to:”
# it might also contain "---"

cleaned_text <- raw_text %>%
  group_by(newsgroup, id) %>%
  filter(cumsum(text == "") > 0,
         cumsum(str_detect(text, "^--")) == 0) %>%
  ungroup()

# Many lines also have nested text representing quotes from other users, 
# We also choose to manually remove two messages, 9704 and 9985 that contained a large amount of non-text content.
cleaned_text <- cleaned_text %>%
  filter(str_detect(text, "^[^>]+[A-Za-z\\d]") | text == "",
         !str_detect(text, "writes(:|\\.\\.\\.)$"),
         !str_detect(text, "^In article <"),
         !id %in% c(9704, 9985))
# we’ve removed the headers, signatures, and formatting
# removing stopwords also

usenet_words <- cleaned_text %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word)


# A lot more fine cleaning can be done ...but I am not moving forward
### Exploring the dataset

usenet_words %>%
  count(word, sort = TRUE)

words_by_newsgroup <- usenet_words %>%
  count(newsgroup, word, sort = TRUE) %>%
  ungroup()

words_by_newsgroup

# Extracting featuring words with the help of tf-idf 
tf_idf <- words_by_newsgroup %>%
  bind_tf_idf(word, newsgroup, n) %>%
  arrange(desc(tf_idf))

tf_idf

#lets visualize what we have got from above code...

# Exploring Sci topics
tf_idf %>%
  filter(str_detect(newsgroup, "^sci\\.")) %>%
  group_by(newsgroup) %>%
  top_n(12, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = newsgroup)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ newsgroup, scales = "free") +
  ylab("tf-idf") +
  coord_flip()

# exploring talk topics
tf_idf %>%
  filter(str_detect(newsgroup, "^talk\\.")) %>%
  group_by(newsgroup) %>%
  top_n(12, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = newsgroup)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ newsgroup, scales = "free") +
  ylab("tf-idf") +
  coord_flip()

# Exploring Computer topics
tf_idf %>%
  filter(str_detect(newsgroup, "^comp\\.")) %>%
  group_by(newsgroup) %>%
  top_n(12, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = newsgroup)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ newsgroup, scales = "free") +
  ylab("tf-idf") +
  coord_flip()

# exploring rec

tf_idf %>%
  filter(str_detect(newsgroup, "^rec\\.")) %>%
  group_by(newsgroup) %>%
  top_n(12, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = newsgroup)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ newsgroup, scales = "free") +
  ylab("tf-idf") +
  coord_flip()

# lets see which newsgroups tended to be similar to each other in text content
# using pair-wise correlation of word frequency within each group
newsgroup_cors <- words_by_newsgroup %>%
  pairwise_cor(newsgroup, word, n, sort = TRUE)

newsgroup_cors

# lets see it by visualizing those connections with a correlation greater than .4
newsgroup_cors %>%
  filter(correlation > .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation, width = correlation)) +
  geom_node_point(size = 6, color = "royalblue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


### Topic Modeling using LDA Algo

# include only words that occur at least 50 times
word_sci_newsgroups <- usenet_words %>%
  filter(str_detect(newsgroup, "^sci")) %>%
  group_by(word) %>%
  mutate(word_total = n()) %>%
  ungroup() %>%
  filter(word_total > 50)

# convert into a document-term matrix
# with document names such as sci.crypt_14147
sci_dtm <- word_sci_newsgroups %>%
  unite(document, newsgroup, id) %>%
  count(document, word) %>%
  cast_dtm(document, word, n)

sci_lda <- LDA(sci_dtm, k = 4, control = list(seed = 2019))

sci_lda %>%
  tidy() %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  ungroup() %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip()

## Key-Findings
#1 Topic 1 certainly represents the sci.space newsgroup 
#2 topic 2 is likely drawn from cryptography

sci_lda %>%
  tidy(matrix = "gamma") %>%
  separate(document, c("newsgroup", "id"), sep = "_") %>%
  mutate(newsgroup = reorder(newsgroup, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ newsgroup) +
  labs(x = "Topic",
       y = "# of messages where this was the highest % topic")

## Key-Issues noted with help of plot
#1 the division of Usenet messages wasn’t as clean as the division of book chapters so it could have lead to high gamma values
#2 isn’t surprising since many of the messages are short and could overlap in terms of common words for example
#2 discussions of space travel could include many of the same words as discussions of electronics


### Sentiment Analysis of the News using use the AFINN sentiment lexicon

newsgroup_sentiments <- words_by_newsgroup %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(newsgroup) %>%
  summarize(score = sum(score * n) / sum(n))

newsgroup_sentiments %>%
  mutate(newsgroup = reorder(newsgroup, score)) %>%
  ggplot(aes(newsgroup, score, fill = score > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("Average sentiment score")

# have you noticed:- “misc.forsale” newsgroup was the most positive as its more likely positive adjective are used to sell

# lets look into deeper which make topic -ve or +ve

contributions <- usenet_words %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(occurences = n(),
            contribution = sum(score))

contributions

contributions %>%
  top_n(25, abs(contribution)) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip()

top_sentiment_words <- words_by_newsgroup %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  mutate(contribution = score * n / sum(n))

top_sentiment_words

top_sentiment_words %>%
  filter(str_detect(newsgroup, "^(talk|alt|misc)")) %>%
  group_by(newsgroup) %>%
  top_n(12, abs(contribution)) %>%
  ungroup() %>%
  mutate(newsgroup = reorder(newsgroup, contribution),
         word = reorder(paste(word, newsgroup, sep = "__"), contribution)) %>%
  ggplot(aes(word, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  facet_wrap(~ newsgroup, scales = "free") +
  coord_flip() +
  labs(x = "",
       y = "Sentiment score * # of occurrences")

#1 This confirms our hypothesis about the “misc.forsale” newsgroup
#2 . An atheism newsgroup is likely to discuss “god” in detail even in a negative context
#3 the negative contribution of the word “gun” to the “talk.politics.guns”
#General Thought:- we should always examine the influential words before interpreting it too deeply.

##  Sentiment analysis by message

sentiment_messages <- usenet_words %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(newsgroup, id) %>%
  summarize(sentiment = mean(score),
            words = n()) %>%
  ungroup() %>%
  filter(words >= 5)


# helps us to  see what the message is 
print_message <- function(group, message_id) {
  result <- cleaned_text %>%
    filter(newsgroup == group, id == message_id, text != "")
  
  cat(result$text, sep = "\n")
}
  
  print_message("rec.sport.hockey", 53560)
# lets see What were the most positive messages?
sentiment_messages %>%
  arrange(desc(sentiment))

print_message("rec.sport.hockey", 53560)

# now the negative ones
sentiment_messages %>%
  arrange(sentiment)
print_message("rec.sport.hockey", 53907)



## it would be more effective when we consider by N-gram analysis

usenet_bigrams <- cleaned_text %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

usenet_bigram_counts <- usenet_bigrams %>%
  count(newsgroup, bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")


# lets see for a list of six words that we suspect are used in negation,
# such as “no”, “not”, and “without”, and visualize the sentiment-associated words

negate_words <- c("not", "without", "no", "can't", "don't", "won't")

usenet_bigram_counts %>%
  filter(word1 %in% negate_words) %>%
  count(word1, word2, wt = n, sort = TRUE) %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  mutate(contribution = score * nn) %>%
  group_by(word1) %>%
  top_n(10, abs(contribution)) %>%
  ungroup() %>%
  mutate(word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>%
  ggplot(aes(word2, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free", nrow = 3) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  xlab("Words preceded by a negation") +
  ylab("Sentiment score * # of occurrences") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()

##Key-note
# largest sources of misidentifying a word as positive come from “don’t want/like/care”,
# and the largest source of incorrectly classified negative sentiment is “no problem”.
