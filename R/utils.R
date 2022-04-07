#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

save_dir_parser <- function(save_path = NULL) {
  if (is.null(save_path)) {
    save_dir <<-
      file.path(getwd(), "output")
    message("save_path n'est pas spécifié. Utilise dossier de sortie par défault: ",
            save_dir)
  } else {
    if (grepl("/", save_path)) {
      save_dir = dirname(filepath)
    } else {
      save_dir = getwd()
    }
  }
  return(save_dir)
}

save_dir_handler <- function(save_dir, overwrite = FALSE) {
  message("Sauvegarde les résultats dans le dossier ", save_dir)
  if (!file.exists(save_dir)) {
    dir.create(save_dir)
  } else {
    if (!overwrite) {
      stop(
        "Le dosser de sauvegarde",
        save_dir,
        " existe dèjà. Veuillez le renommer ou le supprimer ou utilisez overwrite=TRUE."
      )
    }
  }
}

default_output_path <- function(func_name, save_dir) {
  if (func_name == "annotation_udpipe") {
    save_path = file.path(save_dir, "udpipe_corpus_complet.csv")
  } else if (func_name == "regex_corpus_udpipe") {
    save_path = file.path(save_dir, "udpipe_corpus_motifs.csv")
  } else if (func_name == "choix_nb_ngrams") {
    save_path = file.path(save_dir, "corpus_motifs_grams.csv")
  } else if (func_name == "motifs_tf_idf"){
    save_path = file.path(save_dir, "tf-idf.csv")
  } else if (func_name == "motifs_stats"){
    save_path = file.path(save_dir, "motifs_stats.csv")
  } else {
    stop("func_name argument invalide: ", func_name)
  }
  return(save_path)
}

save_data_to_csv <-
  function(data,
           func_name,
           save_path = NULL,
           fileEncoding = "",
           overwrite = FALSE) {
    save_dir = save_dir_parser(save_path)
    dir.create(save_dir)
    if (is.null(save_path)) {
      save_path = default_output_path(func_name, save_dir)
    }
    message("Sauvegarde sortie dans ", save_path)
    if (!file.exists(save_path)) {
      write.csv(data, save_path, fileEncoding = fileEncoding)
    } else {
      if (overwrite) {
        warning(
          "Le fichier ",
          save_path,
          " existe dèjà, écrase et sauve nouveau. Pour éviter ce comportement, utiliser overwrite = FALSE."
        )
        write.csv(data, save_path, fileEncoding = fileEncoding)
      } else {
        stop(
          "Le fichier ",
          save_path,
          " existe dèjà. Veuillez le renommer ou le supprimer ou utilisez overwrite=TRUE."
        )
      }
    }
  }

import_corpus <- function(corpus = NULL,
                          corpus_path = NULL,
                          func_name = NULL) {
  if (is.null(corpus) & is.null(corpus_path)) {
    # Load from default paths
    output_dir = file.path(getwd(), "output")
    if (func_name == "regex_corpus_udpipe") {
      corpus_path = file.path(output_dir, "udpipe_corpus_complet.csv")
    } else if (func_name == "choix_nb_ngrams") {
      corpus_path = file.path(getwd(), "output", "udpipe_corpus_motifs.csv")
    } else if (func_name %in% c("motifs_nuage", "motifs_histogram", "motifs_tf_idf", "motifs_acp", "motifs_stats")) {
      corpus_path = file.path(getwd(), "output", "corpus_motifs_grams.csv")
    }
    else{
      stop("func_name argument invalide: ", func_name)
    }
    message("Loading default corpus from ",
            corpus_path,
            " for ",
            func_name)
    corpus = data.table::fread(corpus_path, encoding = "UTF-8", header = TRUE)
  } else if (is.null(corpus) & (!is.null(corpus_path))) {
    message("Chargement du corpus depuis le fichier ", corpus_path)
    if (file.exists(corpus_path)) {
      corpus = data.table::fread(corpus_path, encoding = "UTF-8", header = TRUE)
      return(corpus)
    } else {
      stop("Le fichier ", corpus_path, " n'existe pas.")
    }
  } else if (!is.null(corpus) & (is.null(corpus_path))) {
    return(corpus)
  } else {
    stopifnot(!is.null(corpus) & (!is.null(corpus_path)))
    stop("Vous ne pouvez pas passer à la fois 'corpus' et 'corpus_path' en argument!")
  }
}