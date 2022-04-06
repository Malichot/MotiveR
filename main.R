#' Titre : Script execution des fonctions
#' Auteurs : Dominique Legallois, Antoine de Sacy
#' Date: 6 octobre 2021.

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

# Clean env
rm(list = ls(all = TRUE))
graphics.off()
# Detach packages to test if relative imports are valid.
lapply(
  paste('package:', names(sessionInfo()$otherPkgs), sep = ""),
  detach,
  character.only = TRUE,
  unload = TRUE
)
# Répertoire de travail :
setwd("/Users/brunospilak/Documents/Perso/Motifs/Motifs")

# Params
path = "./Corpus-torun" # chemin du corpus
save_output = TRUE # Sauvegarde résultats
overwrite = TRUE # Écrase résultats précédents
n_grams = 4 # n-gram encodage

source("R/tag_motif_pipeline.R")
source("Fonctions/choix_nb_ngrams.R")
source("Fonctions/motifs_nuage.R")
source("Fonctions/motifs_tf_idf.R")
source("Fonctions/motifs_histogram.R")

require("dplyr") # need to handle %>% or magrittr ?
# require("udpipe")
# require("tidyverse")
# require("vroom")
# require("stringr")
# require("readr")
# require("data.table")

corpus_motifs = tag_motif_pipeline(path = "./Corpus-torun",
                                   save_output = save_output,
                                   overwrite = overwrite)

# Si nouvelle analyse avec different n gram reprendre d ici

# Choix du nombre de ngrams :
corpus_grams =  choix_nb_ngrams(
  n_grams,
  corpus = corpus_motifs,
  save_output = save_output,
  overwrite = overwrite
)

# create corpus_motifs_grams.csv

# Wordcloud :
# require("tidytext")
# require("ggwordcloud")
# require("RColorBrewer")
# require("reshape2")
# require("ggsci")
motifs_nuage(corpus_grams = corpus_grams,
             nmots = 10,
             freq = "rel")

# Histogrammes :
motifs_histogram(corpus_grams = corpus_grams,
                 nmots = 10,
                 freq = "rel")

# TF-IDF :
motifs_tf_idf(
  corpus_grams = corpus_grams,
  n_motifs = 10,
  plot_type = "group",
  save_output = FALSE,
  overwrite = TRUE
)
