#' Titre : Scripts motifs - Choix ngrams
#' Auteurs : Dominique Legallois, Antoine de Sacy
#' Date: 6 octobre 2021.

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

# Fonction de choix du nb de ngrams (màj septembre 2021) :

choix_nb_ngrams <- function(corpus_motifs) {
  require("dplyr")
  require("tidytext")
  require("tidyverse")
  require("data.table")
  
  # Lecture des données :
  corpus_spec <-
    data.table::fread(
      corpus_motifs,
      encoding = "UTF-8",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  
  # Vérification okazou :
  
  corpus_spec <- corpus_spec[, c('mots', 'motifs', 'Oeuvre')]
  
  ## Retrait des cases vides okazou :
  
  corpus_spec <- corpus_spec[complete.cases(corpus_spec), ]
  
  # Choix du nombre de ngrams :
  
  choix_nb_grams <-
    as.numeric(readline("Sélectionner le nombre de ngrams (2 à 7) et tapez enter"))
  
  if (choix_nb_grams == 2) {
    # bigrams motifs :
    corpus_spec_punct <- corpus_spec  %>%
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
  } else if (choix_nb_grams == 3) {
    # 3-grams motifs :
    corpus_spec_punct <- corpus_spec  %>%
      mutate(next_motif = dplyr::lead(motifs),
             next_motif2 = dplyr::lead(motifs, 2)) %>%
      dplyr::filter(!is.na(next_motif),!is.na(next_motif2)) %>%
      mutate(ngrammotif = paste(motifs, next_motif, next_motif2))
    
    # 3-grams mots :
    corpus_spec_punct <- corpus_spec_punct  %>%
      mutate(next_word = dplyr::lead(mots),
             next_word2 = dplyr::lead(mots, 2)) %>%
      dplyr::filter(!is.na(next_word),!is.na(next_word2)) %>%
      mutate(ngrammot = paste(mots, next_word, next_word2))
    
    # Sélection et renommage des colonnes :
    corpus_spec_punct <-
      corpus_spec_punct[, c("mots", "ngrammot", "ngrammotif", "Oeuvre")]
    names(corpus_spec_punct) <-
      c("mots", "ngrammot", "motifs", "Oeuvre")
  } else if (choix_nb_grams == 4) {
    # 4-grams motifs :
    corpus_spec_punct <- corpus_spec  %>%
      mutate(
        next_motif = dplyr::lead(motifs),
        next_motif2 = dplyr::lead(motifs, 2),
        next_motif3 = dplyr::lead(motifs, 3)
      ) %>%
      dplyr::filter(!is.na(next_motif),
             !is.na(next_motif2),
             !is.na(next_motif3)) %>%
      mutate(ngrammotif = paste(motifs, next_motif, next_motif2, next_motif3))
    
    # 4-grams mots :
    corpus_spec_punct <- corpus_spec_punct  %>%
      mutate(
        next_word = dplyr::lead(mots),
        next_word2 = dplyr::lead(mots, 2),
        next_word3 = dplyr::lead(mots, 3)
      ) %>%
      filter(!is.na(next_word),
             !is.na(next_word2),
             !is.na(next_word3)) %>%
      mutate(ngrammot = paste(mots, next_word, next_word2, next_word3))
    
    # Sélection et renommage des colonnes :
    corpus_spec_punct <-
      corpus_spec_punct[, c("mots", "ngrammot", "ngrammotif", "Oeuvre")]
    names(corpus_spec_punct) <-
      c("mots", "ngrammot", "motifs", "Oeuvre")
  } else if (choix_nb_grams == 5) {
    # Fivegrams motifs :
    corpus_spec_punct <- corpus_spec  %>%
      mutate(
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
      mutate(ngrammotif = paste(motifs, next_motif, next_motif2, next_motif3, next_motif4))
    
    # Fivegrams mots :
    corpus_spec_punct <- corpus_spec_punct  %>%
      mutate(
        next_word = dplyr::lead(mots),
        next_word2 = dplyr::lead(mots, 2),
        next_word3 = dplyr::lead(mots, 3),
        next_word4 = dplyr::lead(mots, 4)
      ) %>%
      dplyr::filter(!is.na(next_word),
             !is.na(next_word2),
             !is.na(next_word3),
             !is.na(next_word4)) %>%
      mutate(ngrammot = paste(mots, next_word, next_word2, next_word3, next_word4))
    
    # Sélection et renommage des colonnes :
    corpus_spec_punct <-
      corpus_spec_punct[, c("mots", "ngrammot", "ngrammotif", "Oeuvre")]
    names(corpus_spec_punct) <-
      c("mots", "ngrammot", "motifs", "Oeuvre")
  } else if (choix_nb_grams == 6) {
    # Sixgrams motifs :
    corpus_spec_punct <- corpus_spec  %>%
      mutate(
        next_motif = dplyr::lead(motifs),
        next_motif2 = dplyr::lead(motifs, 2),
        next_motif3 = dplyr::lead(motifs, 3),
        next_motif4 = dplyr::lead(motifs, 4),
        next_motif5 = dplyr::lead(motifs, 5)
      ) %>%
      filter(
        !is.na(next_motif),
        !is.na(next_motif2),
        !is.na(next_motif3),
        !is.na(next_motif4),
        !is.na(next_motif5)
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
        !is.na(next_word),
        !is.na(next_word2),
        !is.na(next_word3),
        !is.na(next_word4),
        !is.na(next_word5)
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
  } else if (choix_nb_grams == 7) {
    # 7-grams motifs :
    corpus_spec_punct <- corpus_spec  %>%
      mutate(
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
        !is.na(next_word),
        !is.na(next_word2),
        !is.na(next_word3),
        !is.na(next_word4),
        !is.na(next_word5),
        !is.na(next_word6)
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
  
  write.csv(corpus_spec_punct,
            "corpus_motifs_grams.csv",
            fileEncoding = "UTF-8")
  
}

choix_nb_ngrams(path = "~/Documents/Huma-num/2021-2022/Motifs/",
                csv = "Corpus_motifs_UDPipe.csv")
