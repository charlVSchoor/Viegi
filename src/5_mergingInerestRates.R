for (bank in BANKS) {  
  
  load(paste0("./data/output/", bank, "post.RData"))
  
  repo_rates <- read_csv(paste0("./dataFoundation/repo_rates/", bank, "/repo_rates.csv"), col_names = T)
  
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

 df_main <- mpc_topics_gamma %>%
          mutate(document = as.integer(document)) %>%
          spread(topic, gamma) %>% arrange(document)
 
 mpc_extract_clean_merge <- mpc_extract_clean %>%
   mutate(document = as.integer(nr)) %>% 
   select(-c(mpc_communication, nr))

 
 df_main <- full_join(mpc_extract_clean_merge, df_main) 
 
 
 df_main <- left_join(df_main, repo_df, by = "date")
 
 df_main <- df_main %>% select("bank", "month", "date","document","repo_value", everything())

  
#  df_main <- 
 chart.Correlation(df_main[,c(4:8)] , histogram=TRUE, pch=19)
  
}


