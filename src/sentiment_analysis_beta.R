options(repr.plot.width=4, repr.plot.height=3) # set plot size in the notebook


#library(jsonlite)

load("./dataFoundation/trump-tweets.RData")
str(as.tibble(tweets))
tweets <- as.tibble(tweets) 




tweets <- tweets %>% 
  mutate(date = parse_datetime(str_sub(tweets$created_at,5,30), "%b %d %H:%M:%S %z %Y")) %>% 
  mutate(is_prez = (date > ymd(20161108))) %>%
  mutate(month = make_date(year(date), month(date)))

str(tweets)


replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets <- tweets %>% 
  filter(!str_detect(text, "^RT")) %>% # remove retweets
  mutate(text = str_replace_all(text, replace_reg, "")) %>% # remove stuff we don't want like links
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>% # tokenize
  filter(!word %in% stop_words$word, str_detect(word, "[a-z]")) %>% # remove stop words
  select(date,word,is_prez,favorite_count,id_str,month) # choose the variables we need
str(tidy_tweets)

ggplot(tidy_tweets, aes(date, favorite_count)) + geom_line() +
  scale_x_date(format = "%b-%Y") + xlab("") + ylab("Favourates")

#Using sentiment lexicons
#The tidytext package comes with a three existing sentiment lexicons or dictionaries. These describe the emotional content of individual words in different formats, and have been put together manually.
#afinn: a list of words given a positivity score between minus five (negative) and plus five (positive). The words have been manually labelled by Finn Arup Nielsen. See here and here for more details.
#bing: a sentiment lexicon created by Bing Liu and collaborators. A list of words are labelled as "positive" or "negative". More details here.
#nrc: a sentiment lexicon put together by Saif Mohammad and Peter Turney using crowdsourcing on Amazon Mechanical Turk. Words are labelled as "positive" or "negative", but also as "anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", or "trust". A word can receive multiple labels. More details here.

get_sentiments("afinn") %>% head(20)
get_sentiments("bing") %>% head(10)
get_sentiments("nrc") %>% head(10)

# apply the "trump" code with SARB and such. These are neutral statements
tidy_tweets <- tidy_tweets %>% 
  left_join(get_sentiments("bing")) %>% # add sentiments (pos or neg)
  select(word,sentiment,everything()) %>%
  mutate(sentiment = ifelse(word == "trump", NA, sentiment)) %>% # "trump" is a positive word in the bing lexicon!
  mutate(sentiment = ifelse(is.na(sentiment), "neutral", sentiment))

as.tibble(tidy_tweets)

tidy_tweets %>%
  filter(sentiment == "positive") %>%
  count(word) %>%
  arrange(desc(n)) %>%
  filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(reorder(word,n),n)) + geom_col() + coord_flip() + xlab("")

tidy_tweets %>%
  filter(sentiment == "negative") %>%
  count(word) %>%
  arrange(desc(n)) %>%
  filter(rank(desc(n)) <= 20) %>%
  ggplot(aes(reorder(word,n),n)) + geom_col() + coord_flip() + xlab("")


sentiments_per_month <- tidy_tweets %>%
  group_by(month, sentiment) %>%
  summarize(n = n()) 

ggplot(filter(sentiments_per_month, sentiment != "neutral" & sentiment != "negative" ), aes(x = month, y = n, fill = sentiment)) +
  geom_col() 

model <- lm(freq ~ month, data = subset(sentiments_per_month, sentiment == "negative"))
summary(model)


sentiments_per_month <- sentiments_per_month %>% 
  left_join(sentiments_per_month %>% 
              group_by(month) %>% 
              summarise(total = sum(n))) %>%
  mutate(freq = n/total) 

sentiments_per_month %>% filter(sentiment != "neutral") %>%
  ggplot(aes(x = month, y = freq, colour = sentiment)) +
  geom_line() + 
  geom_smooth(aes(colour = sentiment))

sentiments_per_tweet <- tidy_tweets %>%
  group_by(id_str) %>%
  summarize(net_sentiment = (sum(sentiment == "positive") - sum(sentiment == "negative")),
            month = first(month))
as.tibble(sentiments_per_tweet)
tweets %>% 
  left_join(sentiments_per_tweet) %>% 
  arrange(net_sentiment) %>% 
  head(10) %>%
  select(text, net_sentiment, date) 

tweets %>% 
  left_join(sentiments_per_tweet) %>% 
  arrange(desc(net_sentiment)) %>% 
  head(10) %>%
  select(text, net_sentiment, date) 

sentiments_per_tweet %>%
  group_by(month) %>%
  summarize(prop_neg = sum(net_sentiment < 0) / n()) %>%
  ggplot(aes(x = month, y = prop_neg)) +
  geom_line() + geom_smooth()

bigrams_separated  <- tweets %>%
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ")


str(bigrams_separated)


bigrams_separated <- bigrams_separated %>% 
  # add sentiment for word 1
  left_join(get_sentiments("bing"), by = c(word1 = "word")) %>%
  rename(sentiment1 = sentiment) %>%
  mutate(sentiment1 = ifelse(word1 == "trump", NA, sentiment1)) %>%
  mutate(sentiment1 = ifelse(is.na(sentiment1), "neutral", sentiment1)) %>%
  
  # add sentiment for word 1
  left_join(get_sentiments("bing"), by = c(word2 = "word")) %>%
  rename(sentiment2 = sentiment) %>%
  mutate(sentiment2 = ifelse(word2 == "trump", NA, sentiment2)) %>%
  mutate(sentiment2 = ifelse(is.na(sentiment2), "neutral", sentiment2)) %>%
  select(month,word1,word2,sentiment1,sentiment2,everything())

negation_words <- c("not", "no", "never", "without")

# show a few
filter(bigrams_separated, word1 %in% negation_words) %>% 
  head(10) %>% select(month, word1, word2, sentiment1, sentiment2) # for display purposes


bigrams_separated <- bigrams_separated %>%
  
  # create a variable that is the opposite of sentiment2
  mutate(opp_sentiment2 = recode(sentiment2, "positive" = "negative",
                                 "negative" = "positive",
                                 "neutral" = "neutral")) %>%
  
  # reverse sentiment2 if word1 is a negation word
  mutate(sentiment2 = ifelse(word1 %in% negation_words, opp_sentiment2, sentiment2)) %>%
  
  # remove the opposite sentiment variable, which we don't need any more
  select(-opp_sentiment2)

bigrams_separated <- bigrams_separated %>%
  mutate(net_sentiment = (sentiment1 == "positive") + (sentiment2 == "positive") - 
           (sentiment1 == "negative") - (sentiment2 == "negative")) %>%
  unite(bigram, word1, word2, sep = " ", remove = FALSE)

bigrams_separated %>%
  filter(net_sentiment > 0) %>% # get positive bigrams
  count(bigram, sort = TRUE) %>%
  filter(rank(desc(n)) < 20) %>%
  ggplot(aes(reorder(bigram,n),n)) + geom_col() + coord_flip() + xlab("")

bigrams_separated %>%
  filter(net_sentiment < 0) %>% # get negative bigrams
  count(bigram, sort = TRUE) %>%
  filter(rank(desc(n)) < 20) %>%
  ggplot(aes(reorder(bigram,n),n)) + geom_col() + coord_flip() + xlab("")

bigrams_separated %>%
  filter(net_sentiment < 0) %>% # get negative bigrams
  filter(word1 %in% negation_words) %>% # get bigrams where first word is negation
  count(bigram, sort = TRUE) %>%
  filter(rank(desc(n)) < 20) %>%
  ggplot(aes(reorder(bigram,n),n)) + geom_col() + coord_flip() + xlab("")
