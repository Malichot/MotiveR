## Titre : Scripts motifs - Fonction génération de wordclouds
## Auteurs : Dominique Legallois, Antoine de Sacy
## Date : 15 mai 2021.

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

# Entrée : un corpus sous format : mots || motifs || Oeuvre

## Paramètres : 

# path = chemin, préférablement celui où se trouve le csv.
# csv = "corpus_motifs_grams.csv" ==> Sortie du script choix ngrams
# nmots = 55 ==> sélection du nombre de motifs à afficher dans la visualisation.

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 


motifs_nuage <- function(path = "~/Dropbox/2020-2021/Corpus-test-motifs/", 
                         csv = "Corpus_motifs_UDPipe.csv", nmots = 25){
  
  # Librairies :
  
  require("dplyr")
  require("tidytext")
  require("tidyverse")
  require("ggwordcloud")
  require("RColorBrewer")
  require("reshape2")
  require("ggsci")
  require("data.table")
  
  # Lecture des données :
  
  setwd(path)
  corpus_spec <- fread(csv, encoding = "UTF-8", header = TRUE, stringsAsFactors = FALSE)
  
  # Vérification okazou (pb index) :
  corpus <- corpus_spec[,c("mots", "motifs", "Oeuvre")]
  
  ## Retrait des cases vides :
  corpus <- corpus[complete.cases(corpus),]
  
  ## Dénombrement + filtrage éventuel des données : ex : n > 10
  corpus <- corpus %>%
    count(Oeuvre, motifs, sort = TRUE)
  
  ## Ajout d'une colonne total words pour normaliser la fréquence (fréquence relative) :
  
  total_words <- corpus %>%
    group_by(Oeuvre) %>%
    summarize(total = sum(n))
  
  corpus_words_ngrams <- left_join(corpus, total_words, by = "Oeuvre") 
  
  ## Calcul de la fréquence relative :
  
  corpus_words_ngrams$rel_freq <- corpus_words_ngrams$n / corpus_words_ngrams$total
  
  # Ordonnancement par fréquences relatives :
  corpus_words_ngrams <- corpus_words_ngrams[order(corpus_words_ngrams$rel_freq, decreasing = T),] 
  
  ## Visualisation sur les fréquences absolues :
  
  plot_abs <- ggplot(
    corpus[1:nmots,], # TOdo : changer 50 par une variable dans la fonction
    aes(
      label = motifs, size = n,
      x = Oeuvre, color = Oeuvre, fill = Oeuvre 
    )
  ) +
    geom_text_wordcloud_area(shape = "diamond") +
    scale_size_area(max_size = 15) +
    scale_x_discrete() +
    theme_minimal()
  
  ## Visualisation sur les fréquences relatives :
  
  plot_freq <- ggplot(
    corpus_words_ngrams[1:nmots,], # Choix du nombre de motifs à faire apparaître
    aes(
      label = motifs, size = rel_freq,
      x = Oeuvre, color = Oeuvre, fill = Oeuvre 
    )
  ) +
    geom_text_wordcloud_area(shape = "diamond") +
    scale_size_area(max_size = 15) + # à moduler suivant le nb de motifs
    scale_x_discrete() +
    theme_minimal()
  
  
  freq_rel <- as.numeric(readline("Fréquences relatives, tapez 1 et enter \n Fréquences absolues, tapez 2 et enter"))
  
  if(freq_rel == 1){
    return(plot_freq)
    
  }
  
  
  if(freq_rel == 2){
    return(plot_abs)
  }
  
  else{
    print("Votre choix ne correspond pas aux critères binaires proposés...!")
  }
  
}

  
motifs_nuage(path = "~/Desktop/Motifs/", 
             csv = "corpus_motifs_grams.csv", nmots = 25)


