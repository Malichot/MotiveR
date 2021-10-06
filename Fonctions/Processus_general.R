#' Titre : Script execution des fonctions
#' Auteurs : Dominique Legallois, Antoine de Sacy
#' Date: 6 octobre 2021.

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

# Répertoire de travail :

setwd("~/Desktop/Motifs/")

# Import des fonctions : chemin du script Fonctions_motifs.R

source("~/Desktop/Motifs/Fonctions/Fonctions_motifs.R")

# Téléchargement du modèle d'annotation :

# library("udpipe")
# dl <- udpipe_download_model(language = "french")

# Annotation : 
annotation_udpipe(path = "~/Motifs/Corpus_test_motifs/", 
                  model = "~/Motifs/model_udpipe/french-gsd-ud-2.5-191206.udpipe")


# Transformation en motifs UDPipe
regex_corpus_entier_UDPipe(path = "~/Desktop/Motifs/", 
                    corpus = "UDPipe_corpus_complet.csv")

# Choix du nombre de ngrams : 
choix_nb_ngrams(path = "~/Desktop/Motifs/", csv = "Corpus_motifs_UDPipe.csv")

# Wordcloud : 

motifs_nuage(path = "~/Desktop/Motifs/", 
             csv = "Corpus_motifs_UDPipe.csv", 
             nmots = 70)

# Histogrammes : 

motifs_histograms(path = "~/Desktop/Motifs/", 
                  csv = "corpus_motifs_grams.csv", 
                  nmots = 30)

# TF-IDF :
tf_idf_motifs(path = "~/Desktop/Motifs/",
              csv = "corpus_motifs_grams.csv", nombre_motifs = 20)

# ACP :
motifs_acp(path = "~/Desktop/Motifs/", csv = "corpus_motifs_grams.csv", 
           freq_filter = 50, n_obs = "all")


# Spécificités :
calcul_de_specificites(path = "~/Desktop/Motifs/",
                       csv = "corpus_motifs_grams.csv")

# Densité :
motifs_densite(path = "~/Desktop/Motifs/", 
               csv = "corpus_motifs_grams.csv", 
               filtre = "Flaubert-Bovary.txt-ut8.txt", 
               motif1 = "le NC de le NC",
               motif2 = "NC de le NC ,",
               motif3 = "le NC de DETPOSS NC",
               motif4 = "à le NC de le",
               motif5 = "NC de le NC .",
               bd = 4000,
               titre_graphique = "Densité sur cinq motifs - Madame Bovary")

# Statistiques générales :
stats_motifs(path = "~/Desktop/Motifs/", 
             csv = "corpus_motifs_grams.csv")

# Retour aux textes : 
retour_texte_specificites(path = "~/Desktop/Motifs/",
                          csv_corpus_motifs = "corpus_motifs_grams.csv",
                          csv_corpus_specificites = "Corpus_spec_freq.csv", 
                          frequence = 50)

# Retour aux textes à partir d'un motif :
retour_texte_specificites_un_motif(path = "~/Desktop/Motifs/",
                                   csv_corpus_motifs = "corpus_motifs_grams.csv",
                                   csv_corpus_specificites = "Corpus_spec_freq.csv",
                                   motif_cible = "de le NC de le")







