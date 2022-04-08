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
frequence = 3
len_context = 4

library(Motifs)

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

# Calcul de spécificités
calcul_spec_freq = calcul_specificites(
  save_freq = TRUE,
  retrait_frequence_1 = TRUE,
  corpus_grams = corpus_grams,
  save_output = save_output,
  overwrite = overwrite
)

# Retour aux textes
corpus_spec_path = NULL
source("R/utils.R")


corpus_spec = import_table(corpus_spec_path, file_name = "corpus_motifs_spec_freq.csv")
head(corpus_spec)
corpus_spec[,V1:=NULL]
head(corpus_spec)

calcul_spec_freq = retour_texte_specificites(
  frequence = 3,
  len_context = 4,
  n_grams = 4,
  # corpus_grams = corpus_grams,
  # corpus_path = corpus_path,
  # corpus_spec = calcul_spec_freq,
  # corpus_spec_path = "./output/corpus_motifs_spec_freq.csv",
  # corpus_spec_path = "./output/corpus_motifs_spec_freq.csv",
  save_output = save_output,
  overwrite = overwrite
)
