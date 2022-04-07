#' TF-IDF plot
#'
#' Fonction génération TF-IDF
#'
#' @param corpus_grams data.frame sous format mots || motifs || Oeuvre
#'
#' @param corpus_path string chemin du csv contenant les motifs en ngram
#'
#' @param n_motifs int sélection du nombre de motifs à afficher
#'
#' @param plot_type string "group" pour une visualisation groupée, "sep" pour une visualisation séparée

#' @example
#' motifs_tf_idf(corpus_path="corpus_motifs_grams.csv", n_motifs = 20, plot_type="group")
#'
#' @export
motifs_tf_idf <- function(corpus_grams=NULL,
                          corpus_path=NULL, 
                          n_motifs = 20, 
                          plot_type = "group",
                          save_output = FALSE,
                          overwrite=FALSE){
  # Lecture des données :
  if (is.null(corpus_grams) & is.null(corpus_path)) {
    corpus_path = file.path(OUTPUT_DIR, "corpus_motifs_grams.csv")
    message("Chargement du corpus_grams depuis le fichier ", corpus_path)
    if (file.exists(corpus_path)) {
      corpus_grams <-
        data.table::fread(
          corpus_path,
          encoding = "UTF-8",
          header = TRUE,
          stringsAsFactors = FALSE
        )
    } else {
      stop("Le fichier ", corpus_path, " n'existe pas.")
    }
  } else if (is.null(corpus_grams) & (!is.null(corpus_path))) {
    message("Chargement du corpus_grams depuis le fichier ", corpus_path)
    if (file.exists(corpus_path)) {
      corpus_grams <-
        data.table::fread(
          corpus_path,
          encoding = "UTF-8",
          header = TRUE,
          stringsAsFactors = FALSE
        )
    } else {
      stop("Le fichier ", corpus_path, " n'existe pas.")
    }
  } else if (!is.null(corpus_grams) & (is.null(corpus_path))) {
    # nothing to do
  } else {
    stopifnot(!is.null(corpus_grams) & (!is.null(corpus_path)))
    stop("Vous ne pouvez pas passer à la fois 'corpus_grams' et 'corpus_path' en argument.")
  }
  
  # Vérification okazou :
  corpus_grams <- corpus_grams[,c("mots", "motifs", "Oeuvre")]
  
  ## Retrait des cases vides :
  
  corpus_grams <- corpus_grams[complete.cases(corpus_grams),]
  
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
  
  corpus_words_ngrams <- dplyr::left_join(corpus_words_ngrams, total_words) 
  
  corpus_words_ngrams <- corpus_words_ngrams %>%
    tidytext::bind_tf_idf(motifs, Oeuvre, n)
  
  corpus_words_ngrams %>%
    dplyr::select(-total) %>%
    dplyr::arrange(desc(tf_idf))
  
  tf_idf_export <- corpus_words_ngrams %>%
    dplyr::select(-total) %>%
    dplyr::arrange(desc(tf_idf))
  
  # Visualisation :
  if(plot_type == "sep"){
    tf_idf_grid <- corpus_words_ngrams %>%
      dplyr::select(-total) %>%
      dplyr::arrange(dplyr::desc(tf_idf)) %>%
      dplyr::mutate(motifs = factor(motifs, levels = rev(unique(motifs)))) %>% 
      dplyr::group_by(Oeuvre) %>% 
      dplyr::top_n(n_motifs) %>% # À moduler suivant les besoins : > x ; == x ; etc.
      dplyr::ungroup %>%
      ggplot2::ggplot(ggplot2::aes(motifs, tf_idf, fill = Oeuvre)) +
      ggplot2::geom_col(show.legend = T) +
      ggplot2::labs(x = NULL, y = "TF-IDF") +
      ggplot2::facet_wrap(~Oeuvre, ncol = 2, scales = "free") +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal()
    return(tf_idf_grid)
  } else if(plot_type == "group"){
    tf_idf_all <- corpus_words_ngrams %>% 
      dplyr::select(-total) %>%
      dplyr::arrange(dplyr::desc(tf_idf)) %>%
      dplyr::mutate(motifs = factor(motifs, levels = rev(unique(motifs)))) %>% 
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
  
  if (save_output) {
    save_path = file.path(OUTPUT_DIR, "tf-idf.csv")
    message("Sauvegarde des motifs dans ", save_path)
    if (!file.exists(save_path)) {
      write.csv(tf_idf_export, save_path)
    } else {
      if (overwrite) {
        warning("Le fichier ", save_path, " existe dèjà, écrase et sauve nouveau. Pour éviter ce comportement, utiliser overwrite = FALSE.")
        write.csv(tf_idf_export, save_path)
      } else {
        stop("Le fichier ", save_path, " existe dèjà. Veuillez le renommer ou le supprimer ou utilisez overwrite=TRUE.")
      }
    }
  }
}