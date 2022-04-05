#' Titre : Script execution des fonctions
#' Auteurs : Dominique Legallois, Antoine de Sacy
#' Date: 6 octobre 2021.

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

# Répertoire de travail :

setwd(".")

# Import des fonctions : chemin du script Fonctions_motifs.R

source("./Fonctions/Fonctions_motifs.R")

# Téléchargement du modèle d'annotation :

# library("udpipe")
# dl <- udpipe_download_model(language = "french")

# Annotation : 
annotation_udpipe(path = "./Corpus-torun//", 
                  model = "./model_udpipe/french-gsd-ud-2.5-191206.udpipe")
# => create UDPipe_corpus_complet.csv

# Transformation en motifs UDPipe
regex_corpus_entier_UDPipe(path = "./", 
                           corpus = "./UDPipe_corpus_complet.csv")

# Si nouvelle analyse avec different n gram reprendre d ici

# Choix du nombre de ngrams : 
choix_nb_ngrams(path = "./", csv = "./Corpus_motifs_UDPipe.csv")
# create corpus_motifs_grams.csv

# Wordcloud : 
motifs_nuage(path = "./", 
             csv = "./corpus_motifs_grams.csv", 
             nmots = 10)

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







