#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @importFrom stats complete.cases phyper sd xtabs
#' @importFrom utils head tail write.csv read.csv


save_dir_parser <- function(save_path = NULL) {
  if (is.null(save_path)) {
    save_dir <- file.path(getwd(), "output")
  } else {
    if (grepl("/", save_path)) {
      save_dir = dirname(save_path)
    } else {
      save_dir = getwd()
    }
  }
  return(save_dir)
}

save_dir_handler <- function(save_dir, overwrite = FALSE) {
  message("Sauvegarde les resultats dans le dossier ", save_dir)
  if (!file.exists(save_dir)) {
    dir.create(save_dir)
  } else {
    if (!overwrite) {
      stop(
        "Le dosser de sauvegarde",
        save_dir,
        " existe deja. Veuillez le renommer ou le supprimer ou utilisez overwrite=TRUE."
      )
    }
  }
}

save_data_to_csv <-
  function(data,
           file_name = NULL,
           save_path = NULL,
           fileEncoding = "",
           overwrite = FALSE) {
    save_dir = save_dir_parser(save_path)
    if (!dir.exists(save_dir)) {
      dir.create(save_dir)
    }
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
          " existe deja, ecrase et sauve nouveau. Pour eviter ce comportement, utiliser overwrite = FALSE."
        )
        write.csv(data, save_path, fileEncoding = fileEncoding)
      } else {
        stop(
          "Le fichier ",
          save_path,
          " existe deja. Veuillez le renommer ou le supprimer ou utilisez overwrite=TRUE."
        )
      }
    }
  }

get_default_path <- function(file) {
  output_dir = file.path(getwd(), "output")
  path = file.path(output_dir, file)
  return(file)
}

check_object_param <- function(object = NULL,
                               object_path = NULL) {
  if (!is.null(object) & !is.null(object_path)) {
    stop("Vous ne pouvez pas passer a la fois 'object' et 'object_path' en argument!")
  }
}

parse_oeuvre_name <- function(filepath){
  filename = tail(strsplit(filepath, "/")[[1]], n=1)
  return (strsplit(filename, ".txt")[[1]][1])
}

import_table <- function(output_dir, file_name) {
  file_path = file.path(output_dir, file_name)
  message("Chargement de la table par default ",
          file_name,
          " depuis le dossier",
          output_dir)
  object = data.table::fread(file_path, encoding = "UTF-8", header = TRUE)
}