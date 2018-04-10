source("~/Desktop/R_utilities/libraries.R")
rm(list=ls())
# --------------------------------------------------------------------------------------------------- #
# set the parameters of the model
# remove irrelevant words
OWN_STOP_WORDS = c("In", "cent", "4,5", "5,7", "r1290", "2,3", "1,6", "16,5", "93")

# set graphing requirements, number of topics, year dimension and specify the number of doceuments (length of the corpus)

TOP_WORDS_TO_GRAPH = 6
NUM_TOPICS = 30
YEARS = 2013:2018
NUM_DOCUMENTS = 31
BANKS = c("SARB")
set.seed(1234)


# hyperperameters 
  ## alpha = 0.9
  ## beta = 0.8
# --------------------------------------------------------------------------------------------------- #

# creating, cleaning and tidying the shell/house

setwd("~/Documents/GitHub/Viegi/src")

parent.dir = getwd()

foldersToCreate = c("data", "output", "data/input", "data/output",  "output/visualization", "output/visualization/pre","output/visualization/post")


foldersToClean = c("data/input/", "data/output/", "output/visualization/pre/","output/visualization/post/")


for (i in 1:length(foldersToCreate)){
  dir.create(paste(parent.dir, foldersToCreate[i], sep = "/"))
}


for (folder in foldersToClean) {
  files = list.files(path = folder)
  for (file in files){
   filePath = paste0(folder, file)
   file.remove(filePath)
   writeLines(paste0("Removed ",filePath))
  }
}



source("./1_dataProcessing.R")
source("./2_modelLDA.R")
source("./3_dataTransformation.R")
source("./4_prePostVisualisation.R")





