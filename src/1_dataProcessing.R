writeLines(paste0("Running 1_dataProcessing.R"))


# foundation dataframe construction
for (bank in BANKS) {
  
  mpc_extract <- 0
  for (i in YEARS) {
    i <- as.tibble(read_csv(paste0("~/Documents/GitHub/Viegi/src/dataFoundation/text_extract/", bank, "/", i,"/mpc_text_extract_", i,".csv")))
    mpc_extract <- rbind(mpc_extract, i) %>% filter(!is.na(bank), bank != 0)
  }

  
  
  # adding some neccesisties and doing some housekeeping
  mpc_extract_clean <- mpc_extract %>%
    mutate(nr = 1:length(bank),
           bank = as.factor(bank),
           month = as.integer(month),
           mpc_communication = as.character(mpc_communication)) %>%
    select(nr, everything()) 

  
  # tidy dataset, unnested and tokenized words (word|bank)
  tidy_mpc_extract <- mpc_extract_clean %>%
    unnest_tokens(word, mpc_communication, token = "words", to_lower = T) %>% 
    filter(!word %in% OWN_STOP_WORDS) %>% 
    filter(!word %in% stop_words$word) 

  
  # create term document frequencies
  mpc_extract_tdf <- tidy_mpc_extract %>%
    group_by(nr, word) %>%
    summarise(n= n())
  
  # the document-term matrix
  mpc_extract_dtm = mpc_extract_tdf %>%
    cast_dtm(nr, word, n)

  
  
  save(mpc_extract,
       mpc_extract_clean, 
       tidy_mpc_extract, 
       mpc_extract_tdf, 
       mpc_extract_dtm,
       file=paste0("data/input/", bank, "pre.RData"))
}
