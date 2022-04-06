#' Titre : Scripts motifs - Histogramme
#' Auteurs : Dominique Legallois, Antoine de Sacy
#' Date: 6 octobre 2021.

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

# Entrée : un corpus sous format : mots || motifs || Oeuvre

## Paramètres :

# path = chemin, préférablement celui où se trouve le csv.
# csv = "corpus_motifs_grams.csv"==> Sortie du script choix de ngrams.
# nmots = 55 ==> sélection du nombre de motifs à afficher dans la visualisation.

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

motifs_histogram <- function(path = "~/Dropbox/2020-2021/Motifs/",
                             csv = "corpus_motifs_grams.csv",
                             nmots = 25) {
  # Librairies :
  
  require("dplyr")
  require("tidytext")
  require("tidyverse")
  require("RColorBrewer")
  require("reshape2")
  require("ggsci")
  require("data.table")
  require("ggpubr")
  
  # Lecture des données :
  
  setwd(path)
  corpus_grams <-
    fread(
      csv,
      encoding = "UTF-8",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  
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
  
  ## Fréquences absolues :
  
  plot_abs <- ggplot(data = corpus_words_ngrams[1:nmots, ],
                     aes(x = motifs, y = n, fill = Oeuvre)) +
    geom_histogram(stat = "identity",
                   position = "dodge",
                   show.legend = T) +
    theme_minimal() +
    theme(legend.position = "top")
  
  plot_abs <-
    plot_abs + theme(axis.text.x = element_text(angle = 50, hjust = 1))
  
  ## Fréquences relatives
  
  # Réordonnancement de la df par fréquences relatives :
  
  df_freq_rel <- corpus_words_ngrams %>%
    arrange(-rel_freq)
  
  # Visualisation :
  
  plot_freq <- ggplot(data = df_freq_rel[1:nmots, ],
                      aes(x = motifs, y = rel_freq, fill = Oeuvre)) +
    geom_histogram(stat = "identity",
                   position = "dodge",
                   show.legend = T) +
    theme_minimal() +
    theme(legend.position = "top")
  
  plot_freq <-
    plot_freq + theme(axis.text.x = element_text(angle = 50, hjust = 1))
  
  freq_rel <-
    as.numeric(
      readline(
        "Fréquences relatives, tapez 1 et enter \n Fréquences absolues, tapez 2 et enter"
      )
    )
  
  if (freq_rel == 1) {
    return(plot_freq)
    
  }
  
  
  if (freq_rel == 2) {
    return(plot_abs)
  }
  
  else{
    print("Votre choix ne correspond pas aux critères binaires proposés...!")
  }
  
}