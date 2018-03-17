library(tictoc)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(plotly)
library(tm)

OWN_STOP_WORDS = c("In")

setwd("/Users/charlvanschoor/Documents/GitHub/Viegi/src")
writeLines(paste0("Running 1_tidying.R"))

mpc_extract <- read_csv("./text_data/mpc_text_extract.csv")

# adding some neccesisties and doing some housekeeping
mpc_extract <- mpc_extract %>% mutate(nr = 1:length(bank)) %>% as.tibble() %>% select(nr, everything())
print(mpc_extract)


