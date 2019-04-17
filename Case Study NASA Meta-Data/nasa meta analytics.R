library(jsonlite)
library(dplyr)
library(tidyr)
library(tidytext)
library(widyr)

library(ggplot2)
library(igraph)
library(ggraph)
library(topicmodels)



# load the data from tha website
metadata <- fromJSON("https://data.nasa.gov/data.json")
names(metadata$dataset)

# if above link is not working we have object file save ...loading it

load(file = "metadata.rda")
# some large amount of data in metadata 97Mb
names(metadata$dataset)


# At present our focus is on title, description and keywords
class(metadata$dataset$title)
class(metadata$dataset$description)
class(metadata$dataset$keyword)

# Keyword is having list of char vector and rest is having char vector

# now i am going to explore keyword, title, desc seperately so keeping id with them so later i can join them
nasa_title <- tibble(id = metadata$dataset$`_id`$`$oid`, 
                     title = metadata$dataset$title)
head(nasa_title)

nasa_desc <- tibble(id = metadata$dataset$`_id`$`$oid`, 
                    desc = metadata$dataset$description)

nasa_desc %>% 
  select(desc) %>% 
  sample_n(5)

nasa_keyword <- tibble(id = metadata$dataset$`_id`$`$oid`, 
                       keyword = metadata$dataset$keyword) %>%
                   unnest(keyword)

head(nasa_keyword)

# As keyword are assigned by human for purpose so we will remove stopword from it
# For title and desc we will remove stopwords

nasa_title <- nasa_title %>% 
  unnest_tokens(word, title) %>% 
  anti_join(stop_words)

nasa_desc <- nasa_desc %>% 
  unnest_tokens(word, desc) %>% 
  anti_join(stop_words)

nasa_keyword <- nasa_keyword %>%
  unnest(keyword)






## Exploration Starts (Lets see what inside our data)

# lets see the most common words from title and desc

nasa_title %>%
  count(word, sort = TRUE)

nasa_desc %>% 
  count(word, sort = TRUE)

# Removing custom words like version from title and desc

my_stopwords <- tibble(word = c(as.character(1:10), 
                                "v1", "v03", "l2", "l3", "l4", "v5.2.0", 
                                "v003", "v004", "v005", "v006", "v7","nbsp",
                                "amp", "gt", "lt", "timesnewromanpsmt", "font",
                                "td", "li", "br", "tr", "quot", "st", "img",
                                "src", "strong", "http", "file", "files"))
nasa_title <- nasa_title %>% 
  anti_join(my_stopwords)
nasa_desc <- nasa_desc %>% 
  anti_join(my_stopwords)

# most common keywords tagged
nasa_keyword %>% 
  group_by(keyword) %>% 
  count(sort = TRUE)

# due to case duplicates occur "Oceans" and "OCEANS"  ...neturalizing them 

nasa_keyword <- nasa_keyword %>% 
  mutate(keyword = toupper(keyword))






## Find the relation between the words used
set.seed(1234)

title_word_pairs <- nasa_title %>% 
  pairwise_count(word, id, sort = TRUE, upper = FALSE)

head(title_word_pairs)

title_word_pairs %>%
  filter(n >= 250) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()





desc_word_pairs <- nasa_desc %>% 
  pairwise_count(word, id, sort = TRUE, upper = FALSE)

head(desc_word_pairs)

desc_word_pairs %>%
  filter(n >= 5000) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "darkred") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

# As we we have seen that desc is filled with the words like "Data", "Global" this will affect the rest important contributin keyword
# using tf-idf for Feature extration

desc_tf_idf <- nasa_desc %>% 
  count(id, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, id, n)

desc_tf_idf %>% 
  arrange(-tf_idf)



# here we see that  both  "n" and term frequency are equal to '1' so this would create problem
# the above problem is that desc fields has only a single word in them so tf-idf algo will think that is a very important word but thats not true.

desc_tf_idf <- desc_tf_idf %>% 
  filter(!near(tf, 1)) %>%
  arrange(-tf_idf)


# as we have extracted out the important features now lets see what are relavent words to some topic
desc_tf_idf <- full_join(desc_tf_idf, nasa_keyword, by = "id")


desc_tf_idf  %>%
  filter(keyword %in% c("SOLAR ACTIVITY", "CLOUDS", 
                        "SEISMOLOGY", "ASTROPHYSICS",
                        "HUMAN HEALTH", "BUDGET")) %>%
  arrange(desc(tf_idf)) %>%
  group_by(keyword) %>%
  distinct(word, keyword, .keep_all = TRUE) %>%
  top_n(15, tf_idf) %>% 
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  ggplot(aes(word, tf_idf, fill = keyword)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~keyword, ncol = 3, scales = "free") +
  coord_flip() +
  labs(title = "Highest tf-idf words in NASA metadata description fields",
       caption = "NASA metadata from https://data.nasa.gov/data.json",
       x = NULL, y = "tf-idf")



#---------keywords exploration--------

keyword_pairs <- nasa_keyword %>% 
  pairwise_count(keyword, id, sort = TRUE, upper = FALSE)

head(keyword_pairs)

keyword_pairs %>%
  filter(n >= 700) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

# above plot is on basis of frequency so we are not getting the relation.
# so finding the correlation among the words as we don't get any idea from above plot

keyword_cors <- nasa_keyword %>% 
  group_by(keyword) %>%
  filter(n() >= 50) %>%
  pairwise_cor(keyword, id, sort = TRUE, upper = FALSE)

head(keyword_cors)

keyword_cors %>%
  filter(correlation > .6) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()


##  Now Topic Modeling - using 

word_counts <- nasa_desc  %>%
  count(id, word, sort = TRUE) %>%
  ungroup()

word_counts

desc_dtm <- word_counts %>%
  cast_dtm(id, word, n)

desc_dtm

# now we are reaady for topic modeling but the problem comes to us  What should be the value of k???
# at present we don't know that so after trying 8,16,24,32,64.....after 24 was high variation in Gamma 
# so we are taking 24 ....we can further track for more accurate but its very time Consuming and if you don't having background of astro then you can know what I feeling 
#lets see what it shows on 24

# be aware that running this model is time intensive
desc_lda <- LDA(desc_dtm, k = 24, control = list(seed = 1234))
desc_lda


tidy_lda <- tidy(desc_lda)

tidy_lda


# lets see the Top 10 keywords of each topic 
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  mutate(term = factor(paste(term, topic, sep = "__"), 
                       levels = rev(paste(term, topic, sep = "__")))) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(title = "Top 10 terms in each LDA topic",
       x = NULL, y = expression(beta)) +
  facet_wrap(~ topic, ncol = 3, scales = "free")


# let’s examine which topics belong to which document 

lda_gamma <- tidy(desc_lda, matrix = "gamma")

lda_gamma

# lets see how are the probabilities distributed by histogram

ggplot(lda_gamma, aes(gamma)) +
  geom_histogram() +
  scale_y_log10() +
  labs(title = "Distribution of probabilities for all topics",
       y = "Number of documents", x = expression(gamma))

# we are now little deeper 
#lets visualize how the probabilities are distributed within each topic

ggplot(lda_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 3) +
  scale_y_log10() +
  labs(title = "Distribution of probability for each topic",
       y = "Number of documents", x = expression(gamma))

# When we tried options higher than 24 (such as 32 or 64), 
# the distributions for γ started to look very flat toward γ = 1 
# documents were not getting sorted into topics very well.



# Let’s connect these topic models with the keywords and see what relationships we can find.
lda_gamma <- full_join(lda_gamma, nasa_keyword, by = c("document" = "id"))

lda_gamma

top_keywords <- lda_gamma %>% 
  filter(gamma > 0.9) %>% 
  count(topic, keyword, sort = TRUE)

top_keywords

# lets see top keywords for each topic?

top_keywords %>%
  group_by(topic) %>%
  top_n(5, n) %>%
  group_by(topic, keyword) %>%
  arrange(desc(n)) %>%  
  ungroup() %>%
  mutate(keyword = factor(paste(keyword, topic, sep = "__"), 
                          levels = rev(paste(keyword, topic, sep = "__")))) %>%
  ggplot(aes(keyword, n, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  labs(title = "Top keywords for each LDA topic",
       x = NULL, y = "Number of documents") +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  facet_wrap(~ topic, ncol = 3, scales = "free")

## Key-Finding
#1 the keywords for topics 13, 16, and 18
#1 are essentially duplicates of each other (“OCEAN COLOR”, “OCEAN OPTICS”, “OCEANS”)
#1 even more if we were to include topic 11

#2 We see “PROJECT COMPLETED” in topics 9, 10, and 21,
#2 along with the names of NASA laboratories and research centers.

#3 important subject areas that stand out are groups of keywords about
#3 atmospheric science, budget/finance, and population/human dimensions

#4  topic 4 is associated with keywords about population and human dimensions, 
#4 and some of the top terms for that topic are “population”, “international”, “center”, and “university”.



# summary what I have learned from the NASA metadata
# By using a combination of network analysis, tf-idf, and topic modeling,
# we have come to a greater understanding of how datasets are related at NASA
# we have more information now about how keywords are connected to each other and which datasets are likely to be related