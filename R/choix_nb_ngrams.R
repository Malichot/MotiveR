#' Choix ngrams
#'
#' Choix du nb de ngrams sur le corpus de motifs
#'
#' @param n_grams int choix de l'encodage en n-grams
#'
#' @param corpus data.frame corpus_motifs motifs pour chaque corpus
#'
#' @param corpus_path string Chemin du csv contenant les corpus_motifs motifs pour chaque corpus
#'
#' @param save_output boolean: Sauvegarde les résultats
#'
#' @param overwrite boolean: Écrase et sauve de nouveaux les résultats
#'
#' @return DataFrame: corpus_annote avec les columns (mots || lemmes || POS || feats || Oeuvre)
#'
#' @example
#' corpus_annote <- annotation_udpipe("curpus-test")
#'
#' @export
choix_nb_ngrams <-
  function(n_grams,
           corpus = NULL,
           corpus_path = NULL,
           save_output = FALSE,
           overwrite = FALSE) {
    stopifnot(n_grams >= 2)
    # Lecture des données :
    if (is.null(corpus) & is.null(corpus_path)) {
      corpus_path = file.path(OUTPUT_DIR, "UDPipe_corpus_motifs.csv")
      message("Chargement du corpus depuis le fichier ", corpus_path)
      if (file.exists(corpus_path)) {
        corpus <-
          data.table::fread(
            corpus_path,
            encoding = "UTF-8",
            header = TRUE,
            stringsAsFactors = FALSE
          )
      } else {
        stop("Le fichier ", corpus_path, " n'existe pas.")
      }
    } else if (is.null(corpus) & (!is.null(corpus_path))) {
      message("Chargement du corpus depuis le fichier ", corpus_path)
      if (file.exists(corpus_path)) {
        corpus <-
          data.table::fread(
            corpus_path,
            encoding = "UTF-8",
            header = TRUE,
            stringsAsFactors = FALSE
          )
      } else {
        stop("Le fichier ", corpus_path, " n'existe pas.")
      }
    } else if (!is.null(corpus) & (is.null(corpus_path))) {
      # nothing to do
    } else {
      stopifnot(!is.null(corpus) & (!is.null(corpus_path)))
      stop("Vous ne pouvez pas passer à la fois 'corpus' et 'corpus_path' en argument.")
    }
    
    # Vérification okazou :
    corpus <- corpus[, c('mots', 'motifs', 'Oeuvre')]
    
    ## Retrait des cases vides okazou :
    corpus <- corpus[complete.cases(corpus),]
    dplyr::lead(corpus)
    
    if (n_grams == 2) {
      # bigrams motifs :
      corpus_spec_punct <- corpus  %>%
        mutate(next_motif = dplyr::lead(motifs)) %>%
        dplyr::filter(!is.na(next_motif)) %>%
        mutate(ngrammotif = paste(motifs, next_motif))
      
      # bigrams mots :
      corpus_spec_punct <- corpus_spec_punct  %>%
        mutate(next_word = dplyr::lead(mots)) %>%
        dplyr::filter(!is.na(next_word)) %>%
        mutate(ngrammot = paste(mots, next_word))
      
      # Sélection et renommage des colonnes :
      corpus_spec_punct <-
        corpus_spec_punct[, c("mots", "ngrammot", "ngrammotif", "Oeuvre")]
      names(corpus_spec_punct) <-
        c("mots", "ngrammot", "motifs", "Oeuvre")
    } else if (n_grams == 3) {
      # 3-grams motifs :
      corpus_spec_punct <- corpus  %>%
        mutate(next_motif = dplyr::lead(motifs),
               next_motif2 = dplyr::lead(motifs, 2)) %>%
        dplyr::filter(!is.na(next_motif), !is.na(next_motif2)) %>%
        mutate(ngrammotif = paste(motifs, next_motif, next_motif2))
      
      # 3-grams mots :
      corpus_spec_punct <- corpus_spec_punct  %>%
        mutate(next_word = dplyr::lead(mots),
               next_word2 = dplyr::lead(mots, 2)) %>%
        dplyr::filter(!is.na(next_word), !is.na(next_word2)) %>%
        mutate(ngrammot = paste(mots, next_word, next_word2))
      
      # Sélection et renommage des colonnes :
      corpus_spec_punct <-
        corpus_spec_punct[, c("mots", "ngrammot", "ngrammotif", "Oeuvre")]
      names(corpus_spec_punct) <-
        c("mots", "ngrammot", "motifs", "Oeuvre")
    } else if (n_grams == 4) {
      # 4-grams motifs :
      corpus_spec_punct <- corpus  %>%
        mutate(
          next_motif = dplyr::lead(motifs),
          next_motif2 = dplyr::lead(motifs, 2),
          next_motif3 = dplyr::lead(motifs, 3)
        ) %>%
        dplyr::filter(!is.na(next_motif),!is.na(next_motif2),!is.na(next_motif3)) %>%
        mutate(ngrammotif = paste(motifs, next_motif, next_motif2, next_motif3))
      
      # 4-grams mots :
      corpus_spec_punct <- corpus_spec_punct  %>%
        mutate(
          next_word = dplyr::lead(mots),
          next_word2 = dplyr::lead(mots, 2),
          next_word3 = dplyr::lead(mots, 3)
        ) %>%
        filter(!is.na(next_word),!is.na(next_word2),!is.na(next_word3)) %>%
        mutate(ngrammot = paste(mots, next_word, next_word2, next_word3))
      
      # Sélection et renommage des colonnes :
      corpus_spec_punct <-
        corpus_spec_punct[, c("mots", "ngrammot", "ngrammotif", "Oeuvre")]
      names(corpus_spec_punct) <-
        c("mots", "ngrammot", "motifs", "Oeuvre")
    } else if (n_grams == 5) {
      # Fivegrams motifs :
      corpus_spec_punct <- corpus  %>%
        mutate(
          next_motif = dplyr::lead(motifs),
          next_motif2 = dplyr::lead(motifs, 2),
          next_motif3 = dplyr::lead(motifs, 3),
          next_motif4 = dplyr::lead(motifs, 4)
        ) %>%
        dplyr::filter(
          !is.na(next_motif),!is.na(next_motif2),!is.na(next_motif3),!is.na(next_motif4)
        ) %>%
        mutate(ngrammotif = paste(motifs, next_motif, next_motif2, next_motif3, next_motif4))
      
      # Fivegrams mots :
      corpus_spec_punct <- corpus_spec_punct  %>%
        mutate(
          next_word = dplyr::lead(mots),
          next_word2 = dplyr::lead(mots, 2),
          next_word3 = dplyr::lead(mots, 3),
          next_word4 = dplyr::lead(mots, 4)
        ) %>%
        dplyr::filter(!is.na(next_word),!is.na(next_word2),!is.na(next_word3),!is.na(next_word4)) %>%
        mutate(ngrammot = paste(mots, next_word, next_word2, next_word3, next_word4))
      
      # Sélection et renommage des colonnes :
      corpus_spec_punct <-
        corpus_spec_punct[, c("mots", "ngrammot", "ngrammotif", "Oeuvre")]
      names(corpus_spec_punct) <-
        c("mots", "ngrammot", "motifs", "Oeuvre")
    } else if (n_grams == 6) {
      # Sixgrams motifs :
      corpus_spec_punct <- corpus  %>%
        mutate(
          next_motif = dplyr::lead(motifs),
          next_motif2 = dplyr::lead(motifs, 2),
          next_motif3 = dplyr::lead(motifs, 3),
          next_motif4 = dplyr::lead(motifs, 4),
          next_motif5 = dplyr::lead(motifs, 5)
        ) %>%
        filter(
          !is.na(next_motif),!is.na(next_motif2),!is.na(next_motif3),!is.na(next_motif4),!is.na(next_motif5)
        ) %>%
        mutate(
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
        mutate(
          next_word = dplyr::lead(mots),
          next_word2 = dplyr::lead(mots, 2),
          next_word3 = dplyr::lead(mots, 3),
          next_word4 = dplyr::lead(mots, 4),
          next_word5 = dplyr::lead(mots, 5)
        ) %>%
        dplyr::filter(
          !is.na(next_word),!is.na(next_word2),!is.na(next_word3),!is.na(next_word4),!is.na(next_word5)
        ) %>%
        mutate(ngrammot = paste(
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
        mutate(
          next_motif = dplyr::lead(motifs),
          next_motif2 = dplyr::lead(motifs, 2),
          next_motif3 = dplyr::lead(motifs, 3),
          next_motif4 = dplyr::lead(motifs, 4),
          next_motif5 = dplyr::lead(motifs, 5),
          next_motif6 = dplyr::lead(motifs, 6)
        ) %>%
        dplyr::filter(
          !is.na(next_motif),!is.na(next_motif2),!is.na(next_motif3),!is.na(next_motif4),!is.na(next_motif5),!is.na(next_motif6)
        ) %>%
        mutate(
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
        mutate(
          next_word = dplyr::lead(mots),
          next_word2 = dplyr::lead(mots, 2),
          next_word3 = dplyr::lead(mots, 3),
          next_word4 = dplyr::lead(mots, 4),
          next_word5 = dplyr::lead(mots, 5),
          next_word6 = dplyr::lead(mots, 6)
        ) %>%
        dplyr::filter(
          !is.na(next_word),!is.na(next_word2),!is.na(next_word3),!is.na(next_word4),!is.na(next_word5),!is.na(next_word6)
        ) %>%
        mutate(
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
    
    if (save_output) {
      save_path = file.path(OUTPUT_DIR, "corpus_motifs_grams.csv")
      message("Sauvegarde des motifs dans ", save_path)
      if (!file.exists(save_path)) {
        write.csv(corpus_spec_punct, save_path, fileEncoding = "UTF-8")
      } else {
        if (overwrite) {
          warning(
            "Le fichier ",
            save_path,
            " existe dèjà, écrase et sauve nouveau. Pour éviter ce comportement, utiliser overwrite = FALSE."
          )
          write.csv(corpus_spec_punct, save_path, fileEncoding = "UTF-8")
        } else {
          stop(
            "Le fichier ",
            save_path,
            " existe dèjà. Veuillez le renommer ou le supprimer ou utilisez overwrite=TRUE."
          )
        }
      }
    }
    return(corpus_spec_punct)
  }
