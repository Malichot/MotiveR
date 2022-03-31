#' Titre : Scripts motifs - Étiquetage UDPipe
#' Auteurs : Dominique Legallois, Antoine de Sacy
#' Date: 6 octobre 2021.

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

                        # A LIRE AVANT : 

# Télécharger une seule fois le modèle avec la commande suivante :

setwd("~/Desktop/Motifs/")

library("udpipe")
dl <- udpipe_download_model(language = "french")

# Une fois téléchargé, un fichier "french-gsd-ud-2.4-190531.udpipe" apparaît dans votre répertoire.

# path  = chemin du répertoire dans lequel se trouve votre corpus sous format .txt avec un fichier par oeuvre.
# model = chemin vers le modèle téléchargé ci-dessus. 

# Sortie : mots || lemmes || POS || feats || Oeuvre

path = "~/Desktop/Motifs/Corpus-nettoye-05-10-21/"
model = "~/Desktop/Motifs/model_udpipe/french-gsd-ud-2.5-191206.udpipe"

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
annotation_udpipe <- function(path = "~/Documents/Huma-num/2021-2022/Motifs/Corpus-test/", 
                              model = "~/Documents/Huma-num/2021-2022/Motifs/model_udpipe/french-gsd-ud-2.5-191206.udpipe"){
  
  # Librairies: 
  require("udpipe")
  require("tidyverse")
  require("vroom")
  require("stringr")
  # Modèle
  udmodel_french <- udpipe_load_model(file = model)
  setwd(path)
  
  # Fichiers txt :
  list_of_files <- list.files(recursive = TRUE,
                              pattern = "*.txt", 
                              full.names = TRUE)
  
  # Lecture : 
  df <- vroom(list_of_files, id = "FileName", delim = "\n", col_names = "mots", progress = F)
  
  # Correction : ajout d'un saut de ligne en bout pour éviter erreurs étiquetage :
  # Correction encodage apostrophes :
  
  df <- df %>%
    mutate(mots = stringr::str_replace_all(.$mots, "$", "\n")) %>%
    mutate(mots = stringr::str_replace_all(.$mots, "’", "'")) %>%
    mutate(mots = stringr::str_replace_all(.$mots, "'", "'"))
  
  # Retrait des NA dans la colonne mots : 
  
  df = df %>%
    na.omit(df$mots)
  
  # Annotation :
  
  corpus_annote <- udpipe_annotate(udmodel_french, x = df$mots, tagger = "default", parser = "none", trace = TRUE, doc_id = df$FileName)
  
  # Transformation en df : 
  corpus_annote <- as.data.frame(corpus_annote)
  
  # Correction de l'identifiant : retrait "./" au début :
  corpus_annote$doc_id <- gsub("\\.\\/", "", corpus_annote$doc_id)
  
  # Sélection des colonnes qui nous intéressent : 
  corpus_annote_cols <- corpus_annote[,c("token", "lemma", "upos", "feats", "doc_id")]
  colnames(corpus_annote_cols) <- c("mots", "lemmes", "POS", "feats", "Oeuvre")
  
  head(corpus_annote_cols)
  
  # Exportation csv : 
  write.csv(corpus_annote_cols, "UDPipe_corpus_complet.csv", fileEncoding = "UTF-8")
  
}


annotation_udpipe(path = "~/Documents/Huma-num/2021-2022/Motifs/Corpus-test/", 
                  model = "~/Documents/Huma-num/2021-2022/Motifs/model_udpipe/french-gsd-ud-2.5-191206.udpipe")



 
