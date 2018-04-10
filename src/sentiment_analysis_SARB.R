options(repr.plot.width=4, repr.plot.height=3) # set plot size in the notebook


for (bank in BANKS) {
  load(paste0("./data/input/", bank, "pre.RData"))
  repo_rates <- read_csv(paste0("./dataFoundation/repo_rates/", bank, "/repo_rates.csv"), col_names = T)
  
  #replication of number 5_merging... in the list of source files
  repo_df = data.frame(ValueDate = c(),
                       TheValue = c())
  for (i in 1:length(mpc_extract$date)) {
    for (j in 1:length(repo_rates$ValueDate)) {
      if (mpc_extract$date[i] == repo_rates$ValueDate[j]) {
        temp_value = repo_rates[j,]
        repo_df = rbind(repo_df, temp_value)
      } else { next }
    }
  }
  
  names(repo_df) = c("date", "repo_value")
  repo_df <- repo_df %>%  mutate(date = parse_datetime(repo_df$date, "%d/%m/%Y")) %>% 
    mutate(month = make_date(year(date), month(date)))
  
  names(mpc_extract)[4] = "text"
  mpc_extract
  mpc_extract <- mpc_extract %>%  mutate(date = parse_datetime(mpc_extract$date, "%d/%m/%Y")) %>% 
    mutate(month = make_date(year(date), month(date)))
  
  mpc_extract <- left_join(mpc_extract, repo_df, by = c("month", "date"))
  

  unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
  tidy_mpc_extract_sent <- mpc_extract %>% 
    unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>% # tokenize
    filter(!word %in% stop_words$word, str_detect(word, "[a-z]")) # remove stop words
  
  get_sentiments("afinn") %>% head(20)
  get_sentiments("bing") %>% head(10)
  get_sentiments("nrc") %>% head(10)
  
  
  tidy_mpc_extract_sent <- tidy_mpc_extract_sent %>% 
    left_join(get_sentiments("bing")) %>% # add sentiments (pos or neg)
    select(word,sentiment,everything()) %>%
    mutate(sentiment = ifelse(word == "SARB", NA, sentiment)) %>% # "trump" is a positive word in the bing lexicon!
    mutate(sentiment = ifelse(is.na(sentiment), "neutral", sentiment))

  as.tibble(tidy_mpc_extract_sent)
  
  
  
}
