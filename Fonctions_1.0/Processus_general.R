## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

  # Stage motifs - Dominique Legallois, Antoine de Sacy. #
        # Script de test pour toutes les fonctions #

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

# Répertoire de travail :

setwd("~/Dropbox/2020-2021/Git-Motifs/Corpus-test/")

# Import des fonctions : chemin du script Fonctions_motifs.R

source("~/Dropbox/2020-2021/Git-Motifs/Fonctions/Fonctions_motifs.R")

# Téléchargement du modèle d'annotation :

# library("udpipe")
# dl <- udpipe_download_model(language = "french")

# Annotation : 
annotation_udpipe(path = "~/Dropbox/2020-2021/Git-Motifs/Corpus-test/", 
                  model = "~/Dropbox/2019-2020/Stage/french-gsd-ud-2.4-190531.udpipe")


# Transformation en motifs UDPipe
regex_corpus_entier_UDPipe(path = "~/Dropbox/2020-2021/Git-Motifs/", 
                    corpus = "UDPipe_corpus_complet.csv")

# Transformation en motifs Cordial :

regex_corpus_entier_Cordial(path = "~/Dropbox/2019-2020/Stage/Corpus_Retour_au_texte/Cordial/")


# Wordcloud : 
motifs_nuage(path = "~/Dropbox/2020-2021/Git-Motifs/", csv = "Corpus_motifs_UDPipe.csv", nmots = 50)

# TF-IDF :
tf_idf_motifs(path = "~/Dropbox/2020-2021/Git-Motifs/", csv = "Corpus_motifs_UDPipe.csv",
              nombre_motifs = 30)

# AFC :
motifs_afc(path = "~/Dropbox/2020-2021/Git-Motifs/", csv = "Corpus_motifs_UDPipe.csv", 
           nombre_oeuvres = 2, nmotifs = 50, nombre_dimensions = 2)

## Une oeuvre : 
motifs_afc(path = "~/Dropbox/2019-2020/Stage/Corpus/", csv = "Corpus_motifs_UDPipe.csv", 
           nombre_oeuvres = 4, nmotifs = 30, nombre_dimensions = 5, une_oeuvre = "Rigodon.txt")

# Spécificités :
calcul_de_specificites(path = "~/Dropbox/2020-2021/Git-Motifs/", csv = "Corpus_motifs_UDPipe.csv")

# Barycentres : 
barycentre(path = "~/Dropbox/2020-2021/Git-Motifs/", csv = "Corpus_motifs_UDPipe.csv")

# Densité :

motifs_densite(path = "~/Dropbox/2020-2021/Git-Motifs/", csv = "Corpus_motifs_UDPipe.csv", 
               filtre = "Casse_pipe.txt", motif1 = "NC à le NC de", motif2 = "NC de le NC de",
               motif3 = "le NC et le NC", motif4 = "le ADJ NC de le", motif5 = "à le NC ce être",
               bd = 4000, titre_graphique = "Densité sur cinq motifs - Casse-pipe")

# Statistiques générales : 
stats_motifs(path = "~/Dropbox/2019-2020/Stage/Corpus/", csv = "Corpus_motifs_UDPipe.csv")

# Retour aux textes : 
retour_texte_specificites(csv_corpus_motifs = "~/Dropbox/2019-2020/Stage/Corpus/Corpus_motifs_UDPipe.csv", 
                          csv = "~/Dropbox/2019-2020/Stage/Corpus/Corpus_motifs_specificites.csv", frequence = 300)

# Retour aux textes à partir d'un motif :
retour_texte_specificites_un_motif(csv_corpus_motifs = "~/Dropbox/2019-2020/Stage/Corpus/Corpus_motifs_UDPipe.csv",
                                   csv_corpus_specificites = "~/Dropbox/2019-2020/Stage/Corpus/Corpus_motifs_specificites.csv", 
                                   motif_cible = "PRES et on ADV PRES")







