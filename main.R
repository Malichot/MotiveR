#' Titre : Script execution des fonctions
#' Auteurs : Dominique Legallois, Antoine de Sacy
#' Date: 6 octobre 2021.

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

# Clean env
rm(list = ls(all = TRUE))
graphics.off()
# Detach packages
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
# source("Fonctions/Fonctions_motifs.R")

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
motifs_nuage(corpus = corpus_grams, nmots = 10, freq = "rel")

# Histogrammes : 
motifs_histograms(path = "./", 
                  csv = "corpus_motifs_grams.csv", 
                  nmots = 10)

# TF-IDF :
tf_idf_motifs(path =  "./", 
              csv = "corpus_motifs_grams.csv", nombre_motifs = 10)

# ACP :
motifs_acp(path = "./", csv = "corpus_motifs_grams.csv", 
           freq_filter = 3, n_obs = "all")


# Spécificités :
calcul_de_specificites(path = "./",
                       csv = "corpus_motifs_grams.csv",
                       retrait_frequence_1 = TRUE)

# Densité :
# motifs_densite(path = "~/Desktop/Motifs/", 
#                csv = "corpus_motifs_grams.csv", 
#                filtre = "Flaubert-Bovary.txt-ut8.txt", 
#                motif1 = "le NC de le NC",
#                motif2 = "NC de le NC ,",
#                motif3 = "le NC de DETPOSS NC",
#                motif4 = "à le NC de le",
#                motif5 = "NC de le NC .",
#                bd = 4000,
#                titre_graphique = "Densité sur cinq motifs - Madame Bovary")

# Statistiques générales :
# stats_motifs(path = ".", 
#              csv = "corpus_motifs_grams.csv")

# Retour aux textes : 
retour_texte_specificites(path = ".",
                          csv_corpus_motifs = "corpus_motifs_grams.csv",
                          csv_corpus_specificites = "Corpus_spec_freq.csv", 
                          frequence = 3)
# Quelle longueur a votre motif => n_gram defini au dessus

# Retour aux textes à partir d'un motif :
retour_texte_specificites_un_motif(path = ".",
                                   csv_corpus_motifs = "corpus_motifs_grams.csv",
                                   csv_corpus_specificites = "Corpus_spec_freq.csv",
                                   motif_cible = ", à le NC")







