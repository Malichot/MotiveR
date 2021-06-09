## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

  # Stage motifs - Dominique Legallois, Antoine de Sacy. #
        # Script de test pour toutes les fonctions #

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

# Répertoire de travail :

setwd("~/Motifs/")

# Import des fonctions : chemin du script Fonctions_motifs.R

source("~/Motifs/Fonctions_2.0/Fonctions_motifs.R")

# Téléchargement du modèle d'annotation :

# library("udpipe")
# dl <- udpipe_download_model(language = "french")

# Annotation : 
annotation_udpipe(path = "~/Motifs/Corpus_test_motifs/", 
                  model = "~/Motifs/model_udpipe/french-gsd-ud-2.5-191206.udpipe")


# Transformation en motifs UDPipe
regex_corpus_entier_UDPipe(path = "~/Motifs/", 
                    corpus = "UDPipe_corpus_complet.csv")

# Transformation en motifs Cordial :
regex_corpus_entier_Cordial(path = "~/Dropbox/2019-2020/Stage/Corpus_Retour_au_texte/Cordial/")

# Choix du nombre de ngrams : 
choix_nb_ngrams(path = "~/Motifs/", csv = "Corpus_motifs_UDPipe.csv")

# Wordcloud : 
motifs_nuage(path = "~/Motifs/", 
             csv = "Corpus_motifs_UDPipe.csv", 
             nmots = 150)

# Histogrammes : 

motifs_histograms(path = "~/Motifs/", 
                  csv = "corpus_motifs_grams.csv", 
                  nmots = 30)

# TF-IDF :
tf_idf_motifs(path = "~/Motifs/",
              csv = "corpus_motifs_grams.csv", nombre_motifs = 10)

# ACP :
motifs_acp(path = "~/Motifs/", csv = "corpus_motifs_grams.csv", 
           freq_filter = 1, n_obs = 50)


# Spécificités :
calcul_de_specificites(path = "~/Motifs/",
                       csv = "corpus_motifs_grams.csv")

# Barycentres : 
barycentre(path = "~/Dropbox/2020-2021/Motifs/", 
           csv = "corpus_motifs_grams.csv")

# Densité :
motifs_densite(path = "~/Motifs/", 
               csv = "corpus_motifs_grams.csv", 
               filtre = "Flaubert-Bovary.txt", 
               motif1 = "le NC de le NC",
               motif2 = "NC de le NC ,",
               motif3 = "le NC de DETPOSS NC",
               motif4 = "à le NC de le",
               motif5 = "NC de le NC .",
               bd = 4000,
               titre_graphique = "Densité sur cinq motifs - Madame Bovary")

# Statistiques générales :
stats_motifs(path = "~/Dropbox/2020-2021/Motifs/", 
             csv = "corpus_motifs_grams.csv")

# Retour aux textes : 
retour_texte_specificites(path = "~/Dropbox/2020-2021/Motifs/",
                          csv_corpus_motifs = "corpus_motifs_grams.csv",
                          csv_corpus_specificites = "Corpus_spec_freq.csv", 
                          frequence = 50)

# Retour aux textes à partir d'un motif :
retour_texte_specificites_un_motif(path = "~/Dropbox/2020-2021/Motifs/",
                                   csv_corpus_motifs = "corpus_motifs_grams.csv",
                                   csv_corpus_specificites = "Corpus_spec_freq.csv",
                                   motif_cible = "de le NC de le")







