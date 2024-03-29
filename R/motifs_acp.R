#' Prépare ACP
#'
#' Prépare les données pour réaliser une ACP (ex: normalisation et filtrage).
#'
#' @param freq_filter int filtre de fréquence pour alléger les normalisations de fréquences. Les
#' motifs qui ont une fréquence au moins égale a freq_filter sont sélectionnés.
#'
#' @param n_motifs int Nombre maximum de motifs par oeuvre pour réaliser l'ACP. Si spécifié,
#' freq_filter est ignoré.
#'
#' @param corpus_grams data.frame sous format mots || motifs || Oeuvre
#'
#' @param corpus_path string chemin du csv contenant les motifs en ngram
#' 
#' @returns DataFrame: avec en colonne, Oeuvre1 | Oeuvre2 | ... | OeuvreN avec les motifs en ligne.
#'
#' @examples
#' file_path <- system.file("extdata", "example_output", "corpus_motifs_grams.csv",
#' package="MotiveR")
#' corpus_grams <- data.table::fread(file_path, encoding = "UTF-8", header = TRUE)
#' data <- prepare_acp(freq_filter=2, corpus_grams=corpus_grams)
#'
#' @export
prepare_acp <-
  function(freq_filter = 2,
           n_motifs = NULL,
           corpus_grams = NULL,
           corpus_path = NULL) {
    # For R CMD check "no visible binding for global variable"
    motifs <- Oeuvre <- n <- NULL
    
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
      dplyr::count(Oeuvre, motifs, sort = TRUE) 
    
    if (!is.null(n_motifs)){
      # Sélectionne les n_motifs premier motifs pour chaque oeuvre.
      corpus_grams <- corpus_grams %>%
        dplyr::group_by(Oeuvre) %>%
        dplyr::top_n(n_motifs)
    } else {
      # Sélectionne les motifs qui apparaissent au moins freq_filter fois
      corpus_grams <- corpus_grams %>%
        dplyr::filter(n >= freq_filter)
    }
    ## Préparation des données pour normalisation :
    ## lignes = motifs
    ## colonnes = corpus
    # Réf : https://stackoverflow.com/questions/19346066/r-re-arrange-dataframe-some-rows-to-columns
    corpus_norm <-
      stats::xtabs(n ~ motifs + Oeuvre, corpus_grams)
    
    # Normalisation Z-score
    corpus_norm <- (corpus_norm - apply(corpus_norm, 1, mean)) / apply(corpus_norm, 1, sd)

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
    
    corpus_norm = as.data.frame.matrix(corpus_norm)
    return(t(corpus_norm))
  }


#' ACP des motifs
#'
#' Fonction d'analyse des motifs en composantes principales
#'
#' @param plot_type string: "var+motif" plot variables et motifs (défaut) "var" plot variables,
#' "motif" plot motifs
#' 
#' @param freq_filter int filtre de fréquence pour alléger les normalisations de fréquences.
#' Les motifs qui ont une fréquence au moins égale a freq_filter sont sélectionnés.
#'
#' @param n_motifs int Nombre maximum de motifs par oeuvre pour réaliser l'ACP. Si spécifié,
#' freq_filter est ignoré.
#'
#' @param n_obs int Nombre maximum d'observations pour réaliser l'ACP. Si NULL, toutes les
#' observations sont utilisées.
#'
#' @param corpus_grams data.frame sous format mots || motifs || Oeuvre
#'
#' @param corpus_path string chemin du csv contenant les motifs en ngram
#'
#' @returns un ggplot
#' 
#' @examples
#' corpus_path = system.file("extdata", "example_output", package="MotiveR")
#' motifs_acp(freq_filter=2, corpus_path=corpus_path)
#'
#' @export
motifs_acp <-
  function(plot_type = "var+motif",
           freq_filter = 2,
           n_motifs = NULL,
           n_obs = NULL,
           corpus_grams = NULL,
           corpus_path = NULL) {
    
    # Prepare data
    corpus_norm <- prepare_acp(
      freq_filter = freq_filter, n_motifs = n_motifs,  corpus_grams =
        corpus_grams,  corpus_path = corpus_path
    )

    # And now PCA :
    if (is.null(n_obs)) {
      corpus_PCA <-
        stats::prcomp(corpus_norm[1:nrow(corpus_norm), ], scale. = FALSE)
    }
    else {
      if (n_obs > nrow(corpus_norm)){
        stop("n_obs est trop grand! Valeur maximale possible: ", nrow(corpus_norm))
      }
      corpus_PCA <- stats::prcomp(corpus_norm[1:n_obs, ], scale. = FALSE)
    }
    
    # Impression des composants
    factoextra::fviz_eig(corpus_PCA)
    
    if (plot_type == "motif") {
      # Plot observations !
      pca_plot <- factoextra::fviz_pca_ind(
        corpus_PCA,
        col.ind = "coord", # Colorer par le cos2
        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
        repel = FALSE,
      )
    } else if (plot_type == "var") {
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
