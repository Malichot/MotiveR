#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom data.table :=
#' @export
data.table::`:=`

save_dir_parser <- function(save_path = NULL) {
  if (is.null(save_path)) {
    save_dir <- file.path(getwd(), "output")
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

default_output_path <- function(file_name, save_dir) {
  if (file_name == "udpipe_corpus_complet") {
    save_path = file.path(save_dir, "udpipe_corpus_complet.csv")
  } else if (file_name == "udpipe_corpus_motifs") {
    save_path = file.path(save_dir, "udpipe_corpus_motifs.csv")
  } else if (file_name == "corpus_motifs_grams") {
    save_path = file.path(save_dir, "corpus_motifs_grams.csv")
  } else if (file_name == "idf") {
    save_path = file.path(save_dir, "tf-idf.csv")
  } else if (file_name == "motifs_stats") {
    save_path = file.path(save_dir, "motifs_stats.csv")
  } else if (file_name == "calcul_specificites") {
    save_path = file.path(save_dir, "corpus_motifs_specificites.csv")
  } else if (file_name == "retour_texte_specificites") {
    save_path = file.path(save_dir, file_name)
  } else {
    stop("file_name argument invalide: ", file_name)
  }
  return(save_path)
}

save_data_to_csv <-
  function(data,
           file_name = NULL,
           save_path = NULL,
           fileEncoding = "",
           overwrite = FALSE) {
    save_dir = save_dir_parser(save_path)
    dir.create(save_dir)
    if (is.null(save_path)) {
      save_path = file.path(save_dir, file_name)
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

get_default_path <- function(file) {
  output_dir = file.path(getwd(), "output")
  path = file.path(output_dir, file)
  return (file)
}

check_object_param <- function(object = NULL,
                               object_path = NULL) {
  if (!is.null(object) & !is.null(object_path)) {
    stop("Vous ne pouvez pas passer à la fois 'object' et 'object_path' en argument!")
  }
}

import_table <- function(file_path = NULL,
                         file_name = NULL) {
  if (is.null(file_path)) {
    if (is.null(file_name)) {
      stop("Vous devez spécifié file_name")
    }
    # Load from default paths
    output_dir = file.path(getwd(), "output")
    file_path = file.path(output_dir, file_name)
    message("Chargmenet de la table par défault ",
            file_name,
            " depuis ",
            file_path)
    object = data.table::fread(file_path, encoding = "UTF-8", header = TRUE)
  } else {
    message("Chargement de la table depuis ", file_path)
    if (file.exists(file_path)) {
      object = data.table::fread(file_path, encoding = "UTF-8", header = TRUE)
      return(object)
    } else {
      stop("Le fichier ", file_path, " n'existe pas.")
    }
  }
}