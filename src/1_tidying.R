library(tictoc)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(plotly)
library(tm)
library(lubridate)
# --------------------------------- #
OWN_STOP_WORDS = c("In", "cent", "4,5", "5,7", "r1290", "2,3", "1,6", "16,5", "93")
TOP_WORDS_TO_GRAPH = 6
NUM_TOPICS = 30
YEARS = 2013:2018
NUM_DOCUMENTS = 31
# --------------------------------- #
setwd("/Users/charlvanschoor/Documents/GitHub/Viegi/src")
writeLines(paste0("Running 1_tidying.R"))
# --------------------------------- #

# dataframe construction
mpc_extract <- 0
for (i in YEARS) {
  i <- as.tibble(read_csv(paste0("~/Documents/GitHub/Viegi/src/data/text_extract/", i,"/mpc_text_extract_", i,".csv")))
  mpc_extract <- rbind(mpc_extract, i) %>% filter(!is.na(bank), bank != 0)
}
print(mpc_extract)

# adding some neccesisties and doing some housekeeping
mpc_extract_clean <- mpc_extract %>%
  mutate(nr = 1:length(bank),
         bank = as.factor(bank),
         month = as.integer(month),
         mpc_communication = as.character(mpc_communication),
         date = dmy(date)) %>%
  select(nr, everything()) 
print(mpc_extract_clean)

# tidy dataset, unnested and tokenized words (word|bank)
tidy_mpc_extract <- mpc_extract_clean %>%
  unnest_tokens(word, mpc_communication, token = "words", to_lower = T) %>% 
  filter(!word %in% OWN_STOP_WORDS) %>% 
  filter(!word %in% stop_words$word) 
print(tidy_mpc_extract)
str(tidy_mpc_extract)

# words in each document by percentage of appearance (the distribution of the documents):
#### GRAPH 1
tidy_mpc_extract %>% 
  group_by(word, nr) %>%
  summarise(count = n()) %>% 
  ungroup() %>%
  group_by(nr) %>%
  mutate(prop = count/sum(count)) %>%
  ungroup() %>% 
  arrange(nr, -prop) %>% 
  mutate(word = reorder(word, prop), nr = as.factor(nr)) %>% 
  ggplot() + geom_bar(aes(x = word, y = prop*100, fill = word), stat = "identity",  show.legend = FALSE) +
  coord_flip() + facet_wrap(~ nr) + theme(axis.title.y=element_blank(),
                                          axis.text.y=element_blank(),
                                          axis.ticks.y=element_blank())

# most common words in each document by percentage of appearance:
#### GRAPH 2
tidy_mpc_extract %>% 
  group_by(word, nr) %>%
  summarise(count = n()) %>% 
  ungroup() %>%
  group_by(nr) %>%
  mutate(prop = count/sum(count)) %>%
  ungroup() %>% 
  filter(count > 15) %>% 
  filter(!(word %in% OWN_STOP_WORDS)) %>%
  arrange(nr, -prop) %>% 
  mutate(word = reorder(word, prop), nr = as.factor(nr)) %>% 
  ggplot() + geom_bar(aes(x = word, y = prop*100, fill = word), stat = "identity",  show.legend = FALSE) +
  coord_flip() + facet_wrap(~ nr) 


# create term document frequencies
mpc_extract_tdf <- tidy_mpc_extract %>%
  group_by(nr, word) %>%
  summarise(n= n())

# the document-term matrix
mpc_extract_dtm = mpc_extract_tdf %>%
  cast_dtm(nr, word, n)
print(mpc_extract_dtm)

# --------------------------------- #

# LDA
mpc_lda = LDA(mpc_extract_dtm, k = NUM_TOPICS)

# getting the beta matrix - P(word|topic)
mpc_topics_beta = tidy(mpc_lda, matrix = "beta")
print(mpc_topics_beta)

# getting the gamma matrix - P(bank|topic)
mpc_topics_gamma = tidy(mpc_lda, matrix = "gamma")
print(mpc_topics_gamma)

# --------------------------------- #

# graphing top words, by probability, in each topic
# top words
mpc_top_terms <- mpc_topics_beta %>%
  group_by(topic) %>% 
  top_n(TOP_WORDS_TO_GRAPH, beta) %>% 
  ungroup() %>%
  arrange(topic, -beta)
print(mpc_top_terms)

# bottom words
mpc_bottom_terms <- mpc_topics_beta %>%
  group_by(topic) %>% 
  top_n(TOP_WORDS_TO_GRAPH, -beta) %>% 
  ungroup() %>%
  arrange(topic, -beta)
print(mpc_bottom_terms)

#### GRAPH 3
mpc_top_terms %>% 
  mutate(term = reorder(term, beta)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +  theme(axis.title.x=element_blank(),
                        axis.ticks.x=element_blank(),
                        axis.text.x = element_blank())
#### GRAPH 4
mpc_top_terms %>% 
  mutate(term = reorder(term, beta)) %>% 
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_grid(~ topic, scales = "free") +
  coord_flip() +  theme(axis.title.x=element_blank(),
                        axis.ticks.x=element_blank(),
                        axis.text.x = element_blank())

#get topic distributions per document - which top 3 topic does each document likely belong to
mpc_top_documents <- mpc_topics_gamma %>%
  group_by(document) %>% 
  top_n(10, gamma) %>%
  ungroup() %>% 
  mutate(topic = as.factor(topic),
         document = factor(document, levels = c(1:NUM_DOCUMENTS))) %>% 
  arrange(document, topic, -gamma)
print(mpc_top_documents)


#### GRAPH 5
mpc_top_documents %>% 
  ggplot() + geom_col(aes(x=document, y=gamma, col=topic, fill=topic)) +
  theme(axis.text.y = element_blank()) 

#### GRAPH 6
mpc_top_documents %>% 
  mutate(gamma = reorder(gamma, document)) %>% 
  ggplot() + geom_col(aes(x=document, y=gamma, col=topic, fill=topic, alpha = 1/3)) +
  theme(axis.text.y = element_blank()) 


# Next -0 sum all the gammas for each document - get the distributions of each







