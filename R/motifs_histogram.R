#' Histogramme des motifs
#'
#' Fonction génération d'histogramme
#'
#' @param corpus_grams data.frame sous format mots || motifs || Oeuvre
#'
#' @param corpus_path string chemin du csv contenant les motifs en ngram
#'
#' @param nmots int sélection du nombre de motifs à afficher
#'
#' @param freq string "rel" pour fréquence relative ou "abs" pour fréquence absolue

#' @example
#' motifs_histogram(corpus_path="corpus_motifs_grams.csv", nmots = 25)
#'
#' @export
motifs_histogram <- function(corpus_grams = NULL,
                             corpus_path = NULL,
                             nmots = 25,
                             freq = "rel") {
  # require("dplyr")
  # require("tidytext")
  # require("tidyverse")
  # require("RColorBrewer")
  # require("reshape2")
  # require("ggsci")
  # require("data.table")
  # require("ggpubr")
  
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
  corpus_grams <- corpus_grams[, c("mots", "motifs", "Oeuvre")]
  
  ## Retrait des cases vides :
  corpus_grams <- corpus_grams[complete.cases(corpus_grams), ]
  
  ## Dénombrement + filtrage éventuel des données : ex : n > 10
  corpus_grams <- corpus_grams %>%
    count(motifs, Oeuvre, sort = TRUE)
  
  ## Ajout d'une colonne total words pour normaliser la fréquence (fréquence relative) :
  
  total_words <- corpus_grams %>%
    group_by(Oeuvre) %>%
    summarize(total = sum(n))
  
  corpus_words_ngrams <-
    left_join(corpus_grams, total_words, by = "Oeuvre")
  
  ## Calcul de la fréquence relative :
  
  corpus_words_ngrams$rel_freq <-
    corpus_words_ngrams$n / corpus_words_ngrams$total
  
  # Ordonnancement par fréquences relatives :
  corpus_words_ngrams <-
    corpus_words_ngrams[order(corpus_words_ngrams$rel_freq, decreasing = T), ]
  
  # Visualisation en histogrammes :
  
  ## Pour visualisation par fréquences décroissantes :
  
  #Turn your 'treatment' column into a character vector
  corpus_words_ngrams$motifs <-
    as.character(corpus_words_ngrams$motifs)
  #Then turn it back into a factor with the levels in the correct order
  corpus_words_ngrams$motifs <-
    factor(corpus_words_ngrams$motifs,
           levels = unique(corpus_words_ngrams$motifs))
  
  if (freq == "rel") {
    ## Fréquences relatives
    # Réordonnancement de la df par fréquences relatives :
    df_freq_rel <- corpus_words_ngrams %>%
      arrange(-rel_freq)
    # Visualisation :
    hist_plot <- ggplot2::ggplot(data = df_freq_rel[1:nmots, ],
                                 ggplot2::aes(x = motifs, y = rel_freq, fill = Oeuvre)) +
      ggplot2::geom_histogram(stat = "identity",
                              position = "dodge",
                              show.legend = T) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "top")
    
    hist_plot <-
      hist_plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 50, hjust = 1))

  } else if (freq == "abs") {
    ## Fréquences absolues :
    hist_plot <- ggplot2::ggplot(data = corpus_words_ngrams[1:nmots, ],
                                ggplot2::aes(x = motifs, y = n, fill = Oeuvre)) +
      ggplot2::geom_histogram(stat = "identity",
                              position = "dodge",
                              show.legend = T) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "top")
    hist_plot <-
      hist_plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 50, hjust = 1))
  } else{
    stop("L'argument freq=", freq, " n'est pas valide...")
  }
  return (hist_plot)
}