#' Titre : Script execution des fonctions
#' Auteurs : Dominique Legallois, Antoine de Sacy
#' Date: 6 octobre 2021.

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

# Clean env
rm(list = ls(all = TRUE))
graphics.off()
# Detach packages to test if relative imports are valid.

if (!is.null(sessionInfo()$otherPkgs)) {
  lapply(
    paste('package:', names(sessionInfo()$otherPkgs), sep = ""),
    detach,
    character.only = TRUE,
    unload = TRUE
  )
}
# Répertoire de travail :
setwd("/Users/brunospilak/Documents/Perso/Motifs/Motifs")

# Params
path = "./Corpus-torun" # chemin du corpus
save_output = TRUE # Sauvegarde résultats
overwrite = TRUE # Écrase résultats précédents
n_grams = 4 # n-gram encodage
corpus_grams_path = "./output/corpus_motifs_grams.csv"

# source("R/tag_motif_pipeline.R")
# source("R/choix_nb_ngrams.R")
# source("R/motifs_nuage.R")
# source("R/motifs_tf_idf.R")
# source("R/motifs_histogram.R")
library(Motifs)
# require("dplyr") # need to handle %>% or magrittr ?

corpus_annote = annotation_udpipe(path = "./Corpus-torun",
                                  save_output = save_output,
                                  overwrite = overwrite)
corpus_motifs = regex_corpus_udpipe(corpus = corpus_annote,
                                    save_output = save_output,
                                    overwrite = overwrite)

# corpus_motifs = tag_motif_pipeline(path = "./Corpus-torun",
#                                    save_output = save_output,
#                                    overwrite = overwrite)

# Si nouvelle analyse avec different n gram reprendre d ici

# Choix du nombre de ngrams :
corpus_grams =  choix_nb_ngrams(
  n_grams,
  corpus = corpus_motifs,
  save_output = save_output,
  overwrite = overwrite
)

# Wordcloud :
# require("dplyr")
# require("tidytext")
# require("tidyverse")
# require("ggwordcloud")
# require("RColorBrewer")
# require("reshape2")
# require("ggsci")
# require("data.table")
motifs_nuage(corpus_grams = corpus_grams,
             nmots = 10,
             freq = "rel")

# Histogrammes :

# require("dplyr")
# require("tidytext")
# require("tidyverse")
# require("RColorBrewer")
# require("reshape2")
# require("ggsci")
# require("data.table")
# require("ggpubr")

motifs_histogram(corpus_grams = corpus_grams,
                 nmots = 10,
                 freq = "rel")

# TF-IDF :
motifs_tf_idf(
  n_motifs = 10,
  plot_type = "group",
  corpus_grams = corpus_grams,
  save_output = FALSE,
  overwrite = TRUE
)

# ACP :
motifs_acp(plot_type = "var", corpus_grams = corpus_grams)
motifs_acp(plot_type = "motif", corpus_grams = corpus_grams)
motifs_acp(plot_type = "var+motif", corpus_grams = corpus_grams)

# Stats
df_stats = motifs_stats(
  corpus_grams = corpus_grams,
  save_output = save_output,
  overwrite = overwrite
)
