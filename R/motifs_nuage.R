#' Nuage des motifs
#'
#' Fonction génération de wordclouds
#'
#' @param corpus_grams data.frame sous format mots || motifs || Oeuvre
#'
#' @param corpus_path string chemin du csv contenant les motifs en ngram
#'
#' @param nmots int sélection du nombre de motifs à afficher
#'
#' @param freq string "rel" pour fréquence relative ou "abs" pour fréquence absolue
#'
#' @example
#' motifs_nuage(corpus_path="corpus_motifs_grams.csv", nmots = 25, freq = "rel")
#'
#' @export
motifs_nuage <-
  function(corpus_grams = NULL,
           corpus_path = NULL,
           nmots = 25,
           freq = "rel") {
    # Librairies :
    
    # require("dplyr")
    # require("tidytext")
    # require("tidyverse")
    # require("ggwordcloud")
    # require("RColorBrewer")
    # require("reshape2")
    # require("ggsci")
    # require("data.table")
    
    # Lecture des données :
    corpus_grams = import_corpus(corpus_grams, corpus_path, func_name="motifs_nuage")
    
    # Vérification okazou (pb index) :
    corpus_grams <- corpus_grams[, c("mots", "motifs", "Oeuvre")]
    
    ## Retrait des cases vides :
    corpus_grams <- corpus_grams[complete.cases(corpus_grams), ]
    
    ## Dénombrement + filtrage éventuel des données : ex : n > 10
    corpus_grams <- corpus_grams %>%
      count(Oeuvre, motifs, sort = TRUE)
    
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
    
    if (freq == "abs") {
      ## Visualisation sur les fréquences absolues :
      world_cloud_plot <- ggplot2::ggplot(
        corpus_grams[1:nmots, ],
        # TOdo : changer 50 par une variable dans la fonction
        ggplot2::aes(
          label = motifs,
          size = n,
          x = Oeuvre,
          color = Oeuvre,
          fill = Oeuvre
        )
      ) +
        ggwordcloud::geom_text_wordcloud_area(shape = "diamond") +
        ggplot2::scale_size_area(max_size = 15) +
        ggplot2::scale_x_discrete() +
        ggplot2::theme_minimal()
    } else if (freq == "rel") {
      ## Visualisation sur les fréquences relatives :
      world_cloud_plot <- ggplot2::ggplot(
        corpus_words_ngrams[1:nmots, ],
        # Choix du nombre de motifs à faire apparaître
        ggplot2::aes(
          label = motifs,
          size = rel_freq,
          x = Oeuvre,
          color = Oeuvre,
          fill = Oeuvre
        )
      ) +
        ggwordcloud::geom_text_wordcloud_area(shape = "diamond") +
        ggplot2::scale_size_area(max_size = 15) + # à moduler suivant le nb de motifs
        ggplot2::scale_x_discrete() +
        ggplot2::theme_minimal()
    } else {
      stop("L'argument freq=", freq, " n'est pas valide...")
    }
    return(world_cloud_plot)
    
  }
