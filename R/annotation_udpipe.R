#' etiquetage UDPipe
#'
#' etiquetage du corpus situe dans path
#'
#' @param path string: Chemin du dossier contenant les differents corpus.
#'
#' @param save_output boolean: Sauvegarde les resultats
#'
#' @param save_path string: Chemin du fichier de sauvergarde
#'
#' @param overwrite boolean: ecrase et sauve de nouveaux les resultats
#'
#' @returns DataFrame: corpus_annote avec les colonnes (mots || lemmes || POS || feats || Oeuvre)
#'
#' @examples
#' path <- system.file("extdata", "corpus-test", package = "MotiveR")
#' corpus_annote <- annotation_udpipe(path, save_output=FALSE)
#'
#' @export
annotation_udpipe <-
  function(path,
           save_output = TRUE,
           save_path = NULL,
           overwrite = FALSE) {
    # Verifie path
    if (!file.exists(path)) {
      stop("Le chemin specifie ", path, " n'existe pas!")
    }
    # Modele
    UDPIPE_MODEL_PATH <- file.path(system.file("extdata", package = "MotiveR"),
                                   "french-gsd-ud-2.5-191206.udpipe")
    # Si le fichier modele n'existe pas telecharge le
    if (!file.exists(UDPIPE_MODEL_PATH)) {
      message(paste0("Telecharge et sauve le modele dans ", UDPIPE_MODEL_PATH))
      udpipe::udpipe_download_model(language = "french",
                                    model_dir = system.file("extdata", package = "MotiveR"))
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
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "\u0020", " ")) %>% # clean spaces
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "\u00ab", '"')) %>% # clean french guillemets
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "\u00bb", '"')) %>% # clean french guillemets
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "\u201c", '"')) %>% # clean guillemets
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "\u201d", '"')) %>% # clean guillemets
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "\u2019", "'")) %>% # fix apostrophs...
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "\u0027", "'")) %>% # fix apostrophs...
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "(\\.)(\\w)", "\\1 \\2")) %>% # space after . if no
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "(\\!)(\\w)", "\\1 \\2")) %>% # space after ! if no
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "(\\?)(\\w)", "\\1 \\2")) %>% # space after ? if no
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "(\\w)(\\?)", "\\1 \\2")) %>% # space when char is glued to ?
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "(\\w)(\\!)", "\\1 \\2")) %>% # space when char is glued to !
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "(\\w)(\\s?)(\\!)(\\.{3})", "\\1 \\2 \\3")) %>% # space when chars are glued ...!
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "(\\w)(\\s?)(\\.{3})(\\!)", "\\1 \\2 \\3")) %>% # space when chars are glued ...!
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "(\\w)(\\s?)(\\?)(\\.{3})", "\\1 \\2 \\3")) %>% # space when chars are glued ...?
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "(\\w)(\\s?)(\\.{3})(\\?)", "\\1 \\2 \\3")) %>% # space when chars are glued ...?
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "(\\.)(\\\u2014)", "\\1 \\2")) %>% # space after \u2014 if no
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "(\\.)(\\\u2014)", "\\1 \\2")) %>% # space after \u2014 (not the same UNICODE...) if no
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, ":\u2014", ": \u2014")) %>% # Space when :\u2014
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "(\u2014)(\\w)", "\\1 \\2")) %>% # space when \u2014A
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "(\u2014)(\\w)", "\\1 \\2")) %>% # space when \u2014A (not the same UNICODE...) if no
      dplyr::mutate(mots = stringr::str_replace_all(df$mots, "\u2026", "...")) # fix ...
    
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
    
    # Correction de l'identifiant : retrait "./" au debut :
    corpus_annote$doc_id <- gsub("\\.\\/", "", corpus_annote$doc_id)
    
    # Selection des colonnes qui nous interessent :
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
