## Titre : Scripts motifs - Choix nombre ngrams
## Auteurs : Dominique Legallois, Antoine de Sacy
## Date : 15 mai 2021.

choix_nb_ngrams <- function(path = "~/Dropbox/2020-2021/Motifs/",
                            csv = "Corpus_motifs_UDPipe.csv") {
  require("dplyr")
  require("tidytext")
  require("tidyverse")
  require("data.table")
  
  # Lecture des données :
  
  setwd(path)
  
  corpus_spec <-
    fread(
      csv,
      encoding = "UTF-8",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  
  # Vérification okazou :
  
  corpus_spec <- corpus_spec[, c('mots', 'motifs', 'Oeuvre')]
  
  ## Retrait des cases vides okazou :
  
  corpus_spec <- corpus_spec[complete.cases(corpus_spec),]
  
  # Choix du nombre de ngrams :
  
  choix_nb_grams <-
    as.numeric(readline("Sélectionner le nombre de ngrams (2 à 7) et tapez enter"))
  
  if (choix_nb_grams == 2) {
    # bigrams motifs :
    
    corpus_spec_punct <- corpus_spec  %>%
      mutate(next_motif = lead(motifs)) %>%
      filter(!is.na(next_motif)) %>%
      mutate(ngrammotif = paste(motifs, next_motif))
    
    # bigrams mots :
    
    corpus_spec_punct <- corpus_spec_punct  %>%
      mutate(next_word = lead(mots)) %>%
      filter(!is.na(next_word)) %>%
      mutate(ngrammot = paste(mots, next_word))
    
    # Sélection et renommage des colonnes :
    
    corpus_spec_punct <-
      corpus_spec_punct[, c("mots", "ngrammot", "ngrammotif", "Oeuvre")]
    
    names(corpus_spec_punct) <- c("mots", "ngrammot", "motifs", "Oeuvre")
    

  }
  
  if (choix_nb_grams == 3) {
    # 3-grams motifs :
    
    corpus_spec_punct <- corpus_spec  %>%
      mutate(next_motif = lead(motifs),
             next_motif2 = lead(motifs, 2)) %>%
      filter(!is.na(next_motif), !is.na(next_motif2)) %>%
      mutate(ngrammotif = paste(motifs, next_motif, next_motif2))
    
    # 3-grams mots : 
    
    corpus_spec_punct <- corpus_spec_punct  %>%
      mutate(next_word = lead(mots),
             next_word2 = lead(mots, 2)) %>%
      filter(!is.na(next_word), !is.na(next_word2)) %>%
      mutate(ngrammot = paste(mots, next_word, next_word2))
    
    # Sélection et renommage des colonnes :
    
    corpus_spec_punct <-
      corpus_spec_punct[, c("mots", "ngrammot", "ngrammotif", "Oeuvre")]
    
    names(corpus_spec_punct) <- c("mots", "ngrammot", "motifs", "Oeuvre")
    
  }
  
  if (choix_nb_grams == 4) {
    # 4-grams motifs :
    
    corpus_spec_punct <- corpus_spec  %>%
      mutate(
        next_motif = lead(motifs),
        next_motif2 = lead(motifs, 2),
        next_motif3 = lead(motifs, 3)
      ) %>%
      filter(!is.na(next_motif),!is.na(next_motif2),!is.na(next_motif3)) %>%
      mutate(ngrammotif = paste(motifs, next_motif, next_motif2, next_motif3))
    
    # 4-grams mots :
    
    corpus_spec_punct <- corpus_spec_punct  %>%
      mutate(
        next_word = lead(mots),
        next_word2 = lead(mots, 2),
        next_word3 = lead(mots, 3)
      ) %>%
      filter(!is.na(next_word),!is.na(next_word2),!is.na(next_word3)) %>%
      mutate(ngrammot = paste(mots, next_word, next_word2, next_word3))
    
    # Sélection et renommage des colonnes :
    
    corpus_spec_punct <-
      corpus_spec_punct[, c("mots", "ngrammot", "ngrammotif", "Oeuvre")]
    
    names(corpus_spec_punct) <- c("mots", "ngrammot", "motifs", "Oeuvre")
    
  }
  
  if (choix_nb_grams == 5) {
    # Fivegrams motifs :
    
    corpus_spec_punct <- corpus_spec  %>%
      mutate(
        next_motif = lead(motifs),
        next_motif2 = lead(motifs, 2),
        next_motif3 = lead(motifs, 3),
        next_motif4 = lead(motifs, 4)
      ) %>%
      filter(!is.na(next_motif),!is.na(next_motif2),!is.na(next_motif3),!is.na(next_motif4)) %>%
      mutate(ngrammotif = paste(motifs, next_motif, next_motif2, next_motif3, next_motif4))
    
    # Fivegrams mots : 
    
    corpus_spec_punct <- corpus_spec_punct  %>%
      mutate(
        next_word = lead(mots),
        next_word2 = lead(mots, 2),
        next_word3 = lead(mots, 3),
        next_word4 = lead(mots, 4)
      ) %>%
      filter(!is.na(next_word),!is.na(next_word2),!is.na(next_word3),!is.na(next_word4)) %>%
      mutate(ngrammot = paste(mots, next_word, next_word2, next_word3, next_word4))
    
    # Sélection et renommage des colonnes :
    
    corpus_spec_punct <-
      corpus_spec_punct[, c("mots", "ngrammot", "ngrammotif", "Oeuvre")]
    
    names(corpus_spec_punct) <- c("mots", "ngrammot", "motifs", "Oeuvre")
    
    
  }
  
  if (choix_nb_grams == 6) {
    # Sixgrams motifs :
    
    corpus_spec_punct <- corpus_spec  %>%
      mutate(
        next_motif = lead(motifs),
        next_motif2 = lead(motifs, 2),
        next_motif3 = lead(motifs, 3),
        next_motif4 = lead(motifs, 4),
        next_motif5 = lead(motifs, 5)
      ) %>%
      filter(
        !is.na(next_motif),!is.na(next_motif2),!is.na(next_motif3),!is.na(next_motif4),!is.na(next_motif5)
      ) %>%
      mutate(ngrammotif = paste(
        motifs,
        next_motif,
        next_motif2,
        next_motif3,
        next_motif4,
        next_motif5
      ))
    
    # Sixgrams mots :
    
    corpus_spec_punct <- corpus_spec_punct  %>%
      mutate(
        next_word = lead(mots),
        next_word2 = lead(mots, 2),
        next_word3 = lead(mots, 3),
        next_word4 = lead(mots, 4),
        next_word5 = lead(mots, 5)
      ) %>%
      filter(
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
    
    names(corpus_spec_punct) <- c("mots", "ngrammot", "motifs", "Oeuvre")
    
  }
  
  if (choix_nb_grams == 7) {
    # 7-grams motifs :
    
    corpus_spec_punct <- corpus_spec  %>%
      mutate(
        next_motif = lead(motifs),
        next_motif2 = lead(motifs, 2),
        next_motif3 = lead(motifs, 3),
        next_motif4 = lead(motifs, 4),
        next_motif5 = lead(motifs, 5),
        next_motif6 = lead(motifs, 6)
      ) %>%
      filter(
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
        next_word = lead(mots),
        next_word2 = lead(mots, 2),
        next_word3 = lead(mots, 3),
        next_word4 = lead(mots, 4),
        next_word5 = lead(mots, 5),
        next_word6 = lead(mots, 6)
      ) %>%
      filter(
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
    
    names(corpus_spec_punct) <- c("mots", "ngrammot", "motifs", "Oeuvre")
    
  }
  
  write.csv(corpus_spec_punct, "corpus_motifs_grams.csv", fileEncoding = "UTF-8")
  
}


choix_nb_ngrams(path = "~/Dropbox/2020-2021/Motifs/",
                csv = "Corpus_motifs_UDPipe.csv")
