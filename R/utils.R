save_dir_parser <- function(save_path) {
  if (is.null(save_path)) {
    message("save_path n'est pas spécifié")
    save_dir <<-
      file.path(getwd(), paste0("output-", basename(path)))
  } else {
    if (grepl("/", save_path)) {
      save_dir = dirname(filepath)
    } else {
      save_dir = getwd()
    }
  }
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
    save_path = file.path(save_dir, "UDPipe_corpus_complet.csv")
  }
  
  
  return(save_path)
}

save_data_to_csv <-
  function(data, func_name, save_dir, fileEncoding = "UTF-8") {
    if (is.null(save_path)) {
      save_path = default_output_path(func_name, save_dir)
    }
    message("Sauvegarde udpipe annotations dans ", save_path)
    if (!file.exists(save_path)) {
      write.csv(data, save_path, fileEncoding = fileEncoding)
    } else {
      if (overwrite) {
        warning(
          "Le fichier d'annotation ",
          save_path,
          " existe dèjà, écrase et sauve nouveau. Pour éviter ce comportement, utiliser overwrite = FALSE."
        )
        write.csv(data, save_path, fileEncoding = fileEncoding)
      } else {
        stop(
          "Le fichier d'annotation ",
          save_path,
          " existe dèjà. Veuillez le renommer ou le supprimer ou utilisez overwrite=TRUE."
        )
      }
    }
  }