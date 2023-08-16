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
  return(file)
}

check_object_param <- function(object = NULL,
                               object_path = NULL) {
  if (!is.null(object) & !is.null(object_path)) {
    stop("Vous ne pouvez pas passer à la fois 'object' et 'object_path' en argument!")
  }
}

parse_oeuvre_name <- function(filepath){
  filename = tail(strsplit(filepath, "/")[[1]], n=1)
  return (strsplit(filename, ".txt")[[1]][1])
}

import_table <- function(file_path = NULL,
                         file_name = NULL) {
  if (is.null(file_path)) {
    if (is.null(file_name)) {
      stop("Vous devez spécifier file_name")
    }
    # Load from default paths
    output_dir = file.path(getwd(), "output")
    file_path = file.path(output_dir, file_name)
    message("Chargement de la table par défault ",
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