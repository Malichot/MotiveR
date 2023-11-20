#' Étiquetage UDPipe
#'
#' Étiquetage du corpus situé dans path
#'
#' @param path string: Chemin du dossier contenant les différents corpus.
#'
#' @param save_output boolean: Sauvegarde les résultats
#'
#' @param save_path string: Chemin du fichier de sauvergarde
#'
#' @param overwrite boolean: Écrase et sauve de nouveaux les résultats
#'
#' @return DataFrame: corpus_annote avec les columns (mots || lemmes || POS || feats || Oeuvre)
#'
#' @example
#' corpus_annote <- annotation_udpipe("curpus-test")
#'
#' @export
annotation_udpipe <-
  function(path,
           save_output = TRUE,
           save_path = NULL,
           overwrite = FALSE) {
    # Vérfie path
    if (!file.exists(path)) {
      stop("The chemin spécifié ", path, " n'existe pas!")
    }
    # Modele
    UDPIPE_MODEL_PATH <-
      file.path(getwd(), "udpipe", "french-gsd-ud-2.5-191206.udpipe")
    # Si le fichier modele n'existe pas telecharge le
    if (!file.exists(UDPIPE_MODEL_PATH)) {
      message(paste0("Télécharge et sauve le modèle dans ", UDPIPE_MODEL_PATH))
      udpipe::udpipe_download_model(language = "french", model_dir = "./udpipe")
    }
    udmodel_french <-
      udpipe::udpipe_load_model(file = UDPIPE_MODEL_PATH)
    
    # Fichiers txt :
    list_of_files <- list.files(
      path = path,
      recursive = TRUE,
      pattern = "*.txt",
      full.names = TRUE
    )
    
    # Lecture :
    df <-
      vroom::vroom(
        list_of_files,
        id = "FileName",
        delim = "\n",
        col_names = "mots",
        progress = F
      )
    
    # Quelques correction pour ameliorer l'etiquetage et la tokenisation : 
    
    df <- df %>%
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "$", "\n")) %>%
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "\\n", "")) %>% # resolve line breaks in txt...
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "\\s+", " ")) %>% # resolve multiple spaces in txt...
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, " ", " ")) %>% # clean spaces
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "«", '"')) %>% # clean french guillemets
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "»", '"')) %>% # clean french guillemets
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "“", '"')) %>% # clean guillemets
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "”", '"')) %>% # clean guillemets
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "’", "'")) %>% # fix apostrophs...
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "'", "'")) %>% # fix apostrophs...
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "(\\.)(\\w)", "\\1 \\2")) %>% # space after . if no
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "(\\!)(\\w)", "\\1 \\2")) %>% # space after ! if no
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "(\\?)(\\w)", "\\1 \\2")) %>% # space after ? if no
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "(\\w)(\\?)", "\\1 \\2")) %>% # space when char is glued to ?
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "(\\w)(\\!)", "\\1 \\2")) %>% # space when char is glued to !
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "(\\w)(\\s?)(\\!)(\\.{3})", "\\1 \\2 \\3")) %>% # space when chars are glued ...!
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "(\\w)(\\s?)(\\.{3})(\\!)", "\\1 \\2 \\3")) %>% # space when chars are glued ...!
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "(\\w)(\\s?)(\\?)(\\.{3})", "\\1 \\2 \\3")) %>% # space when chars are glued ...?
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "(\\w)(\\s?)(\\.{3})(\\?)", "\\1 \\2 \\3")) %>% # space when chars are glued ...?
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "(\\.)(\\—)", "\\1 \\2")) %>% # space after — if no
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "(\\.)(\\—)", "\\1 \\2")) %>% # space after — (not the same UNICODE...) if no
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, ":–", ": –")) %>% # Space when :–
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "(–)(\\w)", "\\1 \\2")) %>% # space when –A
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "(—)(\\w)", "\\1 \\2")) %>% # space when —A (not the same UNICODE...) if no
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "…", "...")) %>% # fix ...
    
    # Retrait des NA dans la colonne mots :
    
    df = df %>%
      stats::na.omit(df$mots)
    
    # Annotation :
    
    corpus_annote <-
      udpipe::udpipe_annotate(
        udmodel_french,
        x = df$mots,
        tagger = "default",
        parser = "none",
        trace = TRUE,
        doc_id = df$FileName
      )
    
    # Transformation en df :
    corpus_annote <- as.data.frame(corpus_annote)
    
    # Correction de l'identifiant : retrait "./" au début :
    corpus_annote$doc_id <- gsub("\\.\\/", "", corpus_annote$doc_id)
    
    # Sélection des colonnes qui nous intéressent :
    corpus_annote <-
      corpus_annote[, c("token", "lemma", "upos", "feats", "doc_id")]
    colnames(corpus_annote) <-
      c("mots", "lemmes", "POS", "feats", "Oeuvre")
    
    ## Conserve le titre de l'oeuvre seulement au lien du chemin
    corpus_annote[, "Oeuvre"] = sapply(corpus_annote$Oeuvre, parse_oeuvre_name)
    
    # Exportation csv :
    if (!is.null(save_path) | save_output) {
      save_data_to_csv(
        corpus_annote,
        "udpipe_corpus_complet.csv",
        save_path,
        fileEncoding = "UTF-8",
        overwrite = overwrite
      )
    }
    return(corpus_annote)
  }
