for (bank in BANKS) {
  load(paste0("./data/input/", bank, "pre.RData"))
  # LDA
  mpc_lda = LDA(mpc_extract_dtm, k = NUM_TOPICS #, control = list(alpha = 0.9)
  )
  
  save(mpc_lda, file = paste0("data/output/", bank, "post.RData"))
}