#' Choix ngrams
#'
#' Choix du nb de ngrams sur le corpus de motifs
#'
#' @param n_grams int choix de l'encodage en n-grams, entre 2 et 7
#'
#' @param corpus data.frame corpus_motifs motifs pour chaque corpus
#'
#' @param corpus_path string Chemin du csv contenant les corpus_motifs motifs pour chaque corpus
#'
#' @param save_output boolean: Sauvegarde les résultats
#'
#' @param save_path string: Chemin du fichier de sauvergarde
#'
#' @param overwrite boolean: Écrase et sauve de nouveaux les résultats
#'
#' @return DataFrame: corpus_annote avec les columns (mots || lemmes || POS || feats || Oeuvre)
#'
#' @examples
#' corpus_path <- system.file("extdata", "example_output", package = "MotiveR")
#' corpus_grams <- choix_nb_ngrams(2, corpus_path=corpus_path, save_output=FALSE)
#'
#' @export
choix_nb_ngrams <-
  function(n_grams,
           corpus = NULL,
           corpus_path = NULL,
           save_output = FALSE,
           save_path = NULL,
           overwrite = FALSE) {
    # For R CMD check "no visible binding for global variable"
    motifs <- next_motif <- mots <- next_word <- next_motif2 <-NULL
    next_word2 <- next_motif3 <- next_word3 <- next_motif4 <- NULL
    next_word4 <- next_motif5 <- next_word5 <- next_motif6 <- NULL
    next_word6 <- NULL
    
    
    stopifnot((n_grams >= 2) & (n_grams <= 7))
    # Lecture des données: 
    check_object_param(corpus, corpus_path)
    if (is.null(corpus)) {
      corpus = import_table(corpus_path, file_name = "udpipe_corpus_motifs.csv")
    }
    
    # Vérification okazou :
    corpus <- corpus[, c('mots', 'motifs', 'Oeuvre')]
    
    ## Retrait des cases vides okazou :
    corpus <- corpus[stats::complete.cases(corpus), ]

    if (n_grams == 2) {
      # bigrams motifs :
      corpus_spec_punct <- corpus  %>%
        dplyr::mutate(next_motif = dplyr::lead(motifs)) %>%
        dplyr::filter(!is.na(next_motif)) %>%
        dplyr::mutate(ngrammotif = paste(motifs, next_motif))
      
      # bigrams mots :
      corpus_spec_punct <- corpus_spec_punct  %>%
        dplyr::mutate(next_word = dplyr::lead(mots)) %>%
        dplyr::filter(!is.na(next_word)) %>%
        dplyr::mutate(ngrammot = paste(mots, next_word))
      
      # Sélection et renommage des colonnes :
      corpus_spec_punct <-
        corpus_spec_punct[, c("mots", "ngrammot", "ngrammotif", "Oeuvre")]
      names(corpus_spec_punct) <-
        c("mots", "ngrammot", "motifs", "Oeuvre")
    } else if (n_grams == 3) {
      # 3-grams motifs :
      corpus_spec_punct <- corpus  %>%
        dplyr::mutate(next_motif = dplyr::lead(motifs),
                      next_motif2 = dplyr::lead(motifs, 2)) %>%
        dplyr::filter(!is.na(next_motif),!is.na(next_motif2)) %>%
        dplyr::mutate(ngrammotif = paste(motifs, next_motif, next_motif2))
      
      # 3-grams mots :
      corpus_spec_punct <- corpus_spec_punct  %>%
        dplyr::mutate(next_word = dplyr::lead(mots),
                      next_word2 = dplyr::lead(mots, 2)) %>%
        dplyr::filter(!is.na(next_word),!is.na(next_word2)) %>%
        dplyr::mutate(ngrammot = paste(mots, next_word, next_word2))
      
      # Sélection et renommage des colonnes :
      corpus_spec_punct <-
        corpus_spec_punct[, c("mots", "ngrammot", "ngrammotif", "Oeuvre")]
      names(corpus_spec_punct) <-
        c("mots", "ngrammot", "motifs", "Oeuvre")
    } else if (n_grams == 4) {
      # 4-grams motifs :
      corpus_spec_punct <- corpus  %>%
        dplyr::mutate(
          next_motif = dplyr::lead(motifs),
          next_motif2 = dplyr::lead(motifs, 2),
          next_motif3 = dplyr::lead(motifs, 3)
        ) %>%
        dplyr::filter(!is.na(next_motif),
                      !is.na(next_motif2),
                      !is.na(next_motif3)) %>%
        dplyr::mutate(ngrammotif = paste(motifs, next_motif, next_motif2, next_motif3))
      
      # 4-grams mots :
      corpus_spec_punct <- corpus_spec_punct  %>%
        dplyr::mutate(
          next_word = dplyr::lead(mots),
          next_word2 = dplyr::lead(mots, 2),
          next_word3 = dplyr::lead(mots, 3)
        ) %>%
        dplyr::filter(!is.na(next_word),
                      !is.na(next_word2),
                      !is.na(next_word3)) %>%
        dplyr::mutate(ngrammot = paste(mots, next_word, next_word2, next_word3))
      
      # Sélection et renommage des colonnes :
      corpus_spec_punct <-
        corpus_spec_punct[, c("mots", "ngrammot", "ngrammotif", "Oeuvre")]
      names(corpus_spec_punct) <-
        c("mots", "ngrammot", "motifs", "Oeuvre")
    } else if (n_grams == 5) {
      # Fivegrams motifs :
      corpus_spec_punct <- corpus  %>%
        dplyr::mutate(
          next_motif = dplyr::lead(motifs),
          next_motif2 = dplyr::lead(motifs, 2),
          next_motif3 = dplyr::lead(motifs, 3),
          next_motif4 = dplyr::lead(motifs, 4)
        ) %>%
        dplyr::filter(
          !is.na(next_motif),
          !is.na(next_motif2),
          !is.na(next_motif3),
          !is.na(next_motif4)
        ) %>%
        dplyr::mutate(ngrammotif = paste(motifs, next_motif, next_motif2, next_motif3, next_motif4))
      
      # Fivegrams mots :
      corpus_spec_punct <- corpus_spec_punct  %>%
        dplyr::mutate(
          next_word = dplyr::lead(mots),
          next_word2 = dplyr::lead(mots, 2),
          next_word3 = dplyr::lead(mots, 3),
          next_word4 = dplyr::lead(mots, 4)
        ) %>%
        dplyr::filter(!is.na(next_word),
                      !is.na(next_word2),
                      !is.na(next_word3),
                      !is.na(next_word4)) %>%
        dplyr::mutate(ngrammot = paste(mots, next_word, next_word2, next_word3, next_word4))
      
      # Sélection et renommage des colonnes :
      corpus_spec_punct <-
        corpus_spec_punct[, c("mots", "ngrammot", "ngrammotif", "Oeuvre")]
      names(corpus_spec_punct) <-
        c("mots", "ngrammot", "motifs", "Oeuvre")
    } else if (n_grams == 6) {
      # Sixgrams motifs :
      corpus_spec_punct <- corpus  %>%
        dplyr::mutate(
          next_motif = dplyr::lead(motifs),
          next_motif2 = dplyr::lead(motifs, 2),
          next_motif3 = dplyr::lead(motifs, 3),
          next_motif4 = dplyr::lead(motifs, 4),
          next_motif5 = dplyr::lead(motifs, 5)
        ) %>%
        dplyr::filter(
          !is.na(next_motif),
          !is.na(next_motif2),
          !is.na(next_motif3),
          !is.na(next_motif4),
          !is.na(next_motif5)
        ) %>%
        dplyr::mutate(
          ngrammotif = paste(
            motifs,
            next_motif,
            next_motif2,
            next_motif3,
            next_motif4,
            next_motif5
          )
        )
      
      # Sixgrams mots :
      corpus_spec_punct <- corpus_spec_punct  %>%
        dplyr::mutate(
          next_word = dplyr::lead(mots),
          next_word2 = dplyr::lead(mots, 2),
          next_word3 = dplyr::lead(mots, 3),
          next_word4 = dplyr::lead(mots, 4),
          next_word5 = dplyr::lead(mots, 5)
        ) %>%
        dplyr::filter(
          !is.na(next_word),
          !is.na(next_word2),
          !is.na(next_word3),
          !is.na(next_word4),
          !is.na(next_word5)
        ) %>%
        dplyr::mutate(ngrammot = paste(
          mots,
          next_word,
          next_word2,
          next_word3,
          next_word4,
          next_word5
        ))
      
      # Sélection et renommage des colonnes :
      corpus_spec_punct <-
        corpus_spec_punct[, c("mots", "ngrammot", "ngrammotif", "Oeuvre")]
      names(corpus_spec_punct) <-
        c("mots", "ngrammot", "motifs", "Oeuvre")
    } else if (n_grams == 7) {
      # 7-grams motifs :
      corpus_spec_punct <- corpus  %>%
        dplyr::mutate(
          next_motif = dplyr::lead(motifs),
          next_motif2 = dplyr::lead(motifs, 2),
          next_motif3 = dplyr::lead(motifs, 3),
          next_motif4 = dplyr::lead(motifs, 4),
          next_motif5 = dplyr::lead(motifs, 5),
          next_motif6 = dplyr::lead(motifs, 6)
        ) %>%
        dplyr::filter(
          !is.na(next_motif),
          !is.na(next_motif2),
          !is.na(next_motif3),
          !is.na(next_motif4),
          !is.na(next_motif5),
          !is.na(next_motif6)
        ) %>%
        dplyr::mutate(
          ngrammotif = paste(
            motifs,
            next_motif,
            next_motif2,
            next_motif3,
            next_motif4,
            next_motif5,
            next_motif6
          )
        )
      
      # 7-grams mots :
      
      corpus_spec_punct <- corpus_spec_punct  %>%
        dplyr::mutate(
          next_word = dplyr::lead(mots),
          next_word2 = dplyr::lead(mots, 2),
          next_word3 = dplyr::lead(mots, 3),
          next_word4 = dplyr::lead(mots, 4),
          next_word5 = dplyr::lead(mots, 5),
          next_word6 = dplyr::lead(mots, 6)
        ) %>%
        dplyr::filter(
          !is.na(next_word),
          !is.na(next_word2),
          !is.na(next_word3),
          !is.na(next_word4),
          !is.na(next_word5),
          !is.na(next_word6)
        ) %>%
        dplyr::mutate(
          ngrammot = paste(
            mots,
            next_word,
            next_word2,
            next_word3,
            next_word4,
            next_word5,
            next_word6
          )
        )
      
      # Sélection et renommage des colonnes :
      corpus_spec_punct <-
        corpus_spec_punct[, c("mots", "ngrammot", "ngrammotif", "Oeuvre")]
      names(corpus_spec_punct) <-
        c("mots", "ngrammot", "motifs", "Oeuvre")
    }
    
    # Exportation csv :
    if (!is.null(save_path) | save_output) {
      save_data_to_csv(
        corpus_spec_punct,
        "corpus_motifs_grams.csv",
        save_path,
        fileEncoding = "UTF-8",
        overwrite = overwrite
      )
    }
    return(corpus_spec_punct)
  }
