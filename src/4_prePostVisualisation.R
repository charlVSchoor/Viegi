# PRE VISUALS
# words in each document by percentage of appearance (the distribution of the documents):
for (bank in BANKS) {  
  
  load(paste0("./data/output/", bank, "post.RData"))
  
  pdf(paste0("output/visualization/pre/prortion-of-words-in-docs-", bank, ".pdf"), height=10, width=10)
  #### GRAPH word proportion in each document
  print(tidy_mpc_extract %>% 
    group_by(word, nr) %>%
    summarise(count = n()) %>% 
    ungroup() %>%
    group_by(nr) %>%
    mutate(prop = count/sum(count)) %>%
    ungroup() %>% 
    arrange(nr, -prop) %>% as.data.frame %>% 
    mutate(word = reorder(word, prop), nr = as.factor(nr)) %>% 
    ggplot() + geom_bar(aes(x = word, y = prop*100, fill = word), stat = "identity",  show.legend = FALSE) +
    coord_flip() + facet_wrap(~ nr) + theme(axis.title.y=element_blank(),
                                            axis.text.y=element_blank(),
                                            axis.ticks.y=element_blank()))
  dev.off()
  writeLines(paste0("created output/visualization/pre/prortion-of-words-in-docs-", bank, ".pdf"))
  
  
  #### GRAPH most common words in each document by percentage of appearance:
  pdf(paste0("output/visualization/pre/most-common-words-in-docs-", bank, ".pdf"), height=10, width=10)
  print(tidy_mpc_extract %>% 
    group_by(word, nr) %>%
    summarise(count = n()) %>% 
    ungroup() %>%
    group_by(nr) %>%
    mutate(prop = count/sum(count)) %>%
    ungroup() %>% 
    filter(prop > 0.005) %>% 
    filter(!(word %in% OWN_STOP_WORDS)) %>%
    arrange(nr, -prop) %>% 
    mutate(nr = as.factor(nr)) %>% 
    ggplot() + geom_bar(aes(x = word, y = prop*100, fill = word), stat = "identity",  show.legend = FALSE) +
    coord_flip() + facet_wrap(~ nr) 
  )
  dev.off()
  writeLines(paste0("created output/visualization/pre/most-common-words-in-docs-", bank, ".pdf")) 
  
  
  
  
  # POST VISUALS
  
  #### GRAPH 3
  pdf(paste0("output/visualization/post/top-terms-", bank, ".pdf"), height=10, width=10)
  print(mpc_top_terms %>% 
    mutate(term = reorder(term, beta)) %>% 
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +  theme(axis.title.x=element_blank(),
                          axis.ticks.x=element_blank(),
                          axis.text.x = element_blank())
  )
  dev.off()
  writeLines(paste0("created output/visualization/post/top-terms-", bank, ".pdf"))
  #### GRAPH 4
  pdf(paste0("output/visualization/post/bottom-terms-", bank, ".pdf"), height=10, width=10)
  print(mpc_top_terms %>% 
    mutate(term = reorder(term, beta)) %>% 
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_grid(~ topic, scales = "free") +
    coord_flip() +  theme(axis.title.x=element_blank(),
                          axis.ticks.x=element_blank(),
                          axis.text.x = element_blank())
  )
  dev.off()
  writeLines(paste0("created output/visualization/post/bottom-terms-", bank, ".pdf"))
  
  
  
  #### GRAPH 5
  pdf(paste0("output/visualization/post/top-docs-stacked-proportion-", bank, ".pdf"), height=10, width=10)
  print(mpc_top_documents %>% 
    ggplot() + geom_col(aes(x=document, y=gamma, col=topic, fill=topic)) +
    theme(axis.text.y = element_blank()) 
  )
  dev.off()
  writeLines(paste0("created output/visualization/post/top-docs-stacked-proportion-", bank, ".pdf"))
  
  #### GRAPH 6
  pdf(paste0("output/visualization/post/top-docs-stacked-", bank, ".pdf"), height=10, width=10)
  print(mpc_top_documents %>% 
    mutate(gamma = reorder(gamma, document)) %>% 
    ggplot() + geom_col(aes(x=document, y=gamma, col=topic, fill=topic, alpha = 1/3)) +
    theme(axis.text.y = element_blank()) 
  )
  dev.off()
  writeLines(paste0("created output/visualization/post/top-docs-stacked-", bank, ".pdf"))
  
}