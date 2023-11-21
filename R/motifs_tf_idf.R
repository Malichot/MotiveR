#' TF-IDF transformation
#'
#' Fonction génération TF-IDF
#'
#' @param corpus_grams data.frame sous format mots || motifs || Oeuvre
#'
#' @param corpus_path string chemin du csv contenant les motifs en ngram
#'
#' @param save_output boolean: Sauvegarde les résultats
#'
#' @param save_path string: Chemin du fichier de sauvergarde
#' 
#' @param overwrite boolean: Écrase et sauve de nouveaux les résultats
#'
#' @returns Dataframe avec colonnes Oeuvre || motifs || n || tf || idf || tf_idf
#'
#' @examples
#' motifs_tf_idf(corpus_path="./output")
#'
#' @export
motifs_tf_idf <- function(corpus_grams = NULL,
                          corpus_path = NULL,
                          save_output = FALSE,
                          save_path = NULL,
                          overwrite = FALSE) {
  # For R CMD check "no visible binding for global variable"
  motifs <- Oeuvre <- n <- desc <- total <- tf_idf <- NULL
  
  # Lecture des données :
  check_object_param(corpus_grams, corpus_path)
  if (is.null(corpus_grams)) {
    corpus_grams = import_table(corpus_path, file_name = "corpus_motifs_grams.csv")
  }
  
  # Vérification okazou :
  corpus_grams <- corpus_grams[, c("mots", "motifs", "Oeuvre")]
  
  ## Retrait des cases vides :
  
  corpus_grams <- corpus_grams[complete.cases(corpus_grams), ]

  ## Dénombrement + filtrage éventuel des données : ex : n > 10
  corpus_words_ngrams <- corpus_grams %>%
    dplyr::count(Oeuvre, motifs, sort = TRUE) %>%
    dplyr::filter(n > 1)
  
  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##
  
  # TF-IDF :
  
  ## Ajout d'une colonne total words pour normaliser la fréquence (fréquence relative) :
  
  total_words <- corpus_words_ngrams %>%
    dplyr::group_by(Oeuvre) %>%
    dplyr::summarize(total = sum(n))
  
  corpus_words_ngrams <-
    dplyr::left_join(corpus_words_ngrams, total_words)
  
  corpus_words_ngrams <- corpus_words_ngrams %>%
    tidytext::bind_tf_idf(motifs, Oeuvre, n)
  
  corpus_words_ngrams %>%
    dplyr::select(-total) %>%
    dplyr::arrange(desc(tf_idf))
  
  tf_idf_export <- corpus_words_ngrams %>%
    dplyr::select(-total) %>%
    dplyr::arrange(desc(tf_idf))
  
  # Exportation csv :
  if (!is.null(save_path) | save_output) {
    save_data_to_csv(tf_idf_export,
                     "motifs_tf_idf.csv",
                     save_path,
                     overwrite = overwrite)
  }
  return (corpus_words_ngrams)
}

#' Visualisation des TF-IDF
#'
#' Fonction de génération d'histogrammes TF-IDF
#'
#' @param corpus_words_ngrams data.frame sous format mots || motifs || Oeuvre
#' 
#' @param n_motifs int sélection du nombre de motifs à afficher
#'
#' @param plot_type string "group" pour une visualisation groupée, "sep" pour une visualisation séparée (par défaut)
#'
#' @returns un ggplot
#'
#' @examples
#' corpus_words_ngrams = motifs_tf_idf(corpus_path="./output")
#' plot_tf_idf(corpus_words_ngrams, n_motifs = 20, plot_type = "sep")
#'
#' @export
plot_tf_idf <- function(corpus_words_ngrams, n_motifs = 20, plot_type = "sep"){
  # For R CMD check "no visible binding for global variable"
  motifs <- Oeuvre <- n <- desc <- total <- tf_idf <- NULL
  
  # Visualisation :
  if (plot_type == "sep") {
    tf_idf_grid <- corpus_words_ngrams %>%
      dplyr::select(-total) %>%
      dplyr::arrange(dplyr::desc(tf_idf)) %>%
      dplyr::mutate(motifs = factor(motifs, levels = rev(unique(motifs)))) %>%
      dplyr::group_by(Oeuvre) %>%
      dplyr::top_n(n_motifs) %>% # À moduler suivant les besoins : > x ; == x ; etc.
      dplyr::ungroup() %>%
      ggplot2::ggplot(ggplot2::aes(motifs, tf_idf, fill = Oeuvre)) +
      ggplot2::geom_col(show.legend = T) +
      ggplot2::labs(x = NULL, y = "TF-IDF") +
      ggplot2::facet_wrap( ~ Oeuvre, ncol = 2, scales = "free") +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal()
    return(tf_idf_grid)
  } else if (plot_type == "group") {
    tf_idf_all <- corpus_words_ngrams %>%
      dplyr::select(-total) %>%
      dplyr::arrange(dplyr::desc(tf_idf)) %>%
      dplyr::mutate(motifs = factor(motifs, levels = rev(unique(motifs)))) %>%
      dplyr::group_by(Oeuvre) %>%
      dplyr::top_n(n_motifs) %>% # À moduler suivant les besoins : > x ; == x ; etc.
      dplyr::ungroup() %>%
      dplyr::mutate(word = stats::reorder(motifs, tf_idf)) %>%
      ggplot2::ggplot(ggplot2::aes(motifs, tf_idf, fill = Oeuvre)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::ylab("TF-IDF") +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal()
    return(tf_idf_all)
  } else{
    stop("L'argument plot_type=", plot_type, " n'est pas valide...")
  }
}
