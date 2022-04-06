#' Étiquetage UDPipe
#'
#' Étiquetage du corpus situé dans path
#'
#' @param path Chemin du dossier contenant les différents corpus.
#'
#' @param model Chemin du modèle UDPipe, chemin par défault: "./french-gsd-ud-2.5-191206.udpipe".
#' Si NULL, le modèle est d'abord téléchargé.
#'
#'
#' @return DataFrame: corpus_annote avec les columns (mots || lemmes || POS || feats || Oeuvre)
#'
#' @example inst/examples/example_annotation_udpipe.R
#' corpus_annote <- annotation_udpipe("curpus-test")
#'
#' @export
UDPIPE_DIR = file.path(getwd(), "udpipe")
UDPIPE_MODEL_NAME = "french-gsd-ud-2.5-191206.udpipe"
UDPIPE_MODEL_PATH = file.path(UDPIPE_DIR, UDPIPE_MODEL_NAME)
OUTPUT_DIR = file.path(getwd(), "output")
annotation_udpipe <- function(path, save=TRUE, overwrite=FALSE){
  
  # Librairies: 
  # require("udpipe")
  # require("tidyverse")
  # require("vroom")
  # require("stringr")
  # Modèle
  
  # Si le fichier modèle n'existe pas télécharge le
  if (!file.exists(UDPIPE_MODEL_PATH)) {
    message(paste0("Télécharge et sauve le modèle dans ", UDPIPE_MODEL_PATH))
    udpipe::udpipe_download_model(language = "french", model_dir="./udpipe")
  }
  udmodel_french <- udpipe::udpipe_load_model(file = UDPIPE_MODEL_PATH)

  # Fichiers txt :
  list_of_files <- list.files(path=path,
                              recursive = TRUE,
                              pattern = "*.txt", 
                              full.names = TRUE)
  
  # Lecture : 
  df <- vroom::vroom(list_of_files, id = "FileName", delim = "\n", col_names = "mots", progress = F)
  
  # Correction : ajout d'un saut de ligne en bout pour éviter erreurs étiquetage :
  # Correction encodage apostrophes :
  
  df <- df %>%
    mutate(mots = stringr::str_replace_all(.$mots, "$", "\n")) %>%
    mutate(mots = stringr::str_replace_all(.$mots, "’", "'")) %>%
    mutate(mots = stringr::str_replace_all(.$mots, "'", "'"))
  
  # Retrait des NA dans la colonne mots : 
  
  df = df %>%
    stats::na.omit(df$mots)
  
  # Annotation :
  
  corpus_annote <- udpipe::udpipe_annotate(udmodel_french, x = df$mots, tagger = "default", parser = "none", trace = TRUE, doc_id = df$FileName)
  
  # Transformation en df : 
  corpus_annote <- as.data.frame(corpus_annote)
  
  # Correction de l'identifiant : retrait "./" au début :
  corpus_annote$doc_id <- gsub("\\.\\/", "", corpus_annote$doc_id)
  
  # Sélection des colonnes qui nous intéressent : 
  corpus_annote <- corpus_annote[,c("token", "lemma", "upos", "feats", "doc_id")]
  colnames(corpus_annote) <- c("mots", "lemmes", "POS", "feats", "Oeuvre")
  
  # Exportation csv :
  if (save) {
    if (!file.exists(OUTPUT_DIR)) {
      dir.create(OUTPUT_DIR)
    }
    save_path = file.path(OUTPUT_DIR, "UDPipe_corpus_complet.csv")
    print(paste0("Sauve annotation dans ", save_path))
    if (!file.exists(save_path)) {
      write.csv(corpus_annote, save_path, fileEncoding = "UTF-8")
    } else {
      if (overwrite) {
        warning("Le fichier d'annotation ", save_path, " existe dèjà, écrase et sauve nouveau. Pour éviter ce comportement, utiliser overwrite = FALSE.")
        write.csv(corpus_annote, save_path, fileEncoding = "UTF-8")
      } else {
        stop("Le fichier d'annotation ", save_path, " existe dèjà. Veuillez le renommer ou le supprimer ou utilisez overwrite=TRUE.")
      }
    }
  }
  return(corpus_annote)
}
