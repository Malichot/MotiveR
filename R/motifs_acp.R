#' ACP des motifs
#'
#' Fonction d'analyse des motifs en composantes principales
#'
#' @param corpus_grams data.frame sous format mots || motifs || Oeuvre
#'
#' @param corpus_path string chemin du csv contenant les motifs en ngram
#'
#' @param plot_type string: "var+motif" plot variables et motifs (défaut) "var" plot variables, "motif" plot motifs
#'
#' @param freq_filter int  filtre de fréquence pour alléger les normalisations de fréquences.
#'
#' @param n_obs int Nombre maximum d'observations pour réaliser l'ACP. Si NULL, toutes les observations sont utilisées.

#' @example
#' motifs_acp(corpus_path="corpus_motifs_grams.csv", freq_filter = 2, n_obs=50)
#'
#' @export
motifs_acp <-
  function(plot_type = "var+motif",
           freq_filter = 1,
           n_obs = NULL,
           corpus_grams = NULL,
           corpus_path = NULL) {
    # Lecture des données :
    check_object_param(corpus_grams, corpus_path)
    if (is.null(corpus_grams)) {
      corpus_grams = import_table(corpus_path, file_name = "corpus_motifs_grams.csv")
    }
    
    # Vérification okazou :
    corpus_grams <- corpus_grams[, c("mots", "motifs", "Oeuvre")]
    
    ## Retrait des cases vides :
    corpus_grams <- corpus_grams[complete.cases(corpus_grams), ]
    
    ## Dénombrement + filtrage des données pour alléger les normalisations :
    corpus_grams <- corpus_grams %>%
      dplyr::count(Oeuvre, motifs, sort = TRUE) %>%
      dplyr::filter(n > freq_filter)
    
    ## Préparation des données pour normalisation :
    ## lignes = motifs
    ## colonnes = corpus
    ## Réf : https://stackoverflow.com/questions/19346066/r-re-arrange-dataframe-some-rows-to-columns
    corpus_lexical_table <-
      stats::xtabs(n ~ motifs + Oeuvre, corpus_grams)
    
    ## Ré-ordonnancement :
    corpus_lexical_table <-
      corpus_lexical_table[order(-corpus_lexical_table[, 1], corpus_lexical_table[, 1]), ]
    
    # head(corpus_lexical_table)
    # tail(corpus_lexical_table)
    
    ## Normalisations (zscores)
    ## Cf.
    # Z-scores sur les fréquences de motifs
    ZTransf = function(x) {
      for (i in 1:nrow(x)) {
        x[i, ] = (x[i, ] - mean(x[i, ]))  / sd(x[i, ])
      }
      return(x)
    }
    corpus_norm <- ZTransf(corpus_lexical_table)
    
    # Check na and infinite values :
    a <- is.infinite(corpus_norm)
    b <- which(a == TRUE)
    if (length(b) > 0) {
      corpus_norm <- corpus_norm[-b, ]
    }
    c <- is.na(corpus_norm)
    d <- which(c == TRUE)
    if (length(d) > 0) {
      corpus_norm <- corpus_norm[-d, ]
    }
    
    # And now PCA :
    if (is.null(n_obs)) {
      corpus_PCA <-
        stats::prcomp(corpus_norm[1:nrow(corpus_norm), ], scale. = FALSE)
    }
    else {
      corpus_PCA <- stats::prcomp(corpus_norm[1:n_obs, ], scale. = FALSE)
    }
    
    # Impression des composants
    factoextra::fviz_eig(corpus_PCA)
    
    if (plot_type == "var") {
      # Plot observations !
      pca_plot <- factoextra::fviz_pca_ind(
        corpus_PCA,
        col.ind = "coord",
        # Colorer par le cos2
        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
        repel = FALSE,
      )
    } else if (plot_type == "motif") {
      # Plot variables !
      pca_plot <- factoextra::fviz_pca_var(
        corpus_PCA,
        col.var = "coord",
        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
        repel = TRUE
      )
      
    } else if (plot_type == "var+motif") {
      # Plot variables + obs !
      pca_plot <- factoextra::fviz_pca_biplot(
        corpus_PCA,
        repel = TRUE,
        col.var = "#2E9FDF",
        col.ind = "#696969"
      )
    } else{
      stop("L'argument plot_type=", plot_type, " n'est pas valide...")
    }
    return(pca_plot)
  }
