for (bank in BANKS) {  
  
  load(paste0("./data/output/", bank, "post.RData"))
  
  # getting the beta matrix - P(word|topic)
  mpc_topics_beta = tidy(mpc_lda, matrix = "beta")

  
  # getting the gamma matrix - P(bank|topic)
  mpc_topics_gamma = tidy(mpc_lda, matrix = "gamma")

  
  # top words
  mpc_top_terms <- mpc_topics_beta %>%
    group_by(topic) %>% 
    top_n(TOP_WORDS_TO_GRAPH, beta) %>% 
    ungroup() %>%
    arrange(topic, -beta)

  
  # bottom words
  mpc_bottom_terms <- mpc_topics_beta %>%
    group_by(topic) %>% 
    top_n(TOP_WORDS_TO_GRAPH, -beta) %>% 
    ungroup() %>%
    arrange(topic, -beta)

  #get topic distributions per document - which top 3 topic does each document likely belong to
  mpc_top_documents <- mpc_topics_gamma %>%
    group_by(document) %>% 
    top_n(10, gamma) %>%
    ungroup() %>% 
    mutate(topic = as.factor(topic),
           document = factor(document, levels = c(1:NUM_DOCUMENTS))) %>% 
    arrange(document, topic, -gamma)



  save(mpc_topics_beta,
       mpc_topics_gamma,
       mpc_top_terms,
       mpc_bottom_terms,
       mpc_top_documents,
       file = paste0("data/output/", bank, "postTransformed.RData"))  
}



