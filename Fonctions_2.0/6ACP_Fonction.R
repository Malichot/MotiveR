## Titre : Scripts motifs - Fonction ACP
## Auteurs : Dominique Legallois, Antoine de Sacy
## Date : 15 mai 2021.

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

# Paramètres :

# path = "~/Dropbox/2020-2021/Motifs/" (chemin où se trouve le csv)
# csv = "corpus_motifs_grams.csv" (sortie script choix ngrams)
# freq_filter = filtre de fréquence pour alléger les normalisations de fréquences.

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

motifs_acp <- function(path = "~/Dropbox/2020-2021/Motifs/", csv = "corpus_motifs_grams.csv", 
                       freq_filter = 1){
  
  # Librairies : 
  
  require("tidyverse")
  require("tidytext")
  require("dplyr")
  require("slider")
  require("FactoMineR")
  require("ggplot2")
  require("ggrepel")
  require("ca")
  require("factoextra")
  require("data.table")
  
  # Lecture des données :
  
  setwd(path)
  corpus_grams <- fread(csv, encoding = "UTF-8", header = TRUE, stringsAsFactors = FALSE)
  
  # Vérification okazou :
  corpus_grams <- corpus_grams[,c("mots", "motifs", "Oeuvre")]
  
  ## Retrait des cases vides :
  
  corpus_grams <- corpus_grams[complete.cases(corpus_grams),]
  
  ## Dénombrement + filtrage des données pour alléger les normalisations :
  corpus_grams <- corpus_grams %>%
    dplyr::count(Oeuvre, motifs, sort = TRUE) %>%
    filter(n > freq_filter)
  
  ## Préparation des données pour normalisation : 
  ## lignes = motifs
  ## colonnes = corpus
  ## Réf : https://stackoverflow.com/questions/19346066/r-re-arrange-dataframe-some-rows-to-columns
  
  corpus_lexical_table <- xtabs(n~motifs+Oeuvre, corpus_grams)
  
  ## Ré-ordonnancement : 
  
  corpus_lexical_table <- corpus_lexical_table[order(-corpus_lexical_table[,1], corpus_lexical_table[,1]),]
  
  head(corpus_lexical_table)
  tail(corpus_lexical_table)

  ## Normalisations (zscores)
  ## Cf.
  
  # Z-scores sur les fréquences de motifs
  ZTransf = function(x){
    for(i in 1:nrow(x)){
      x[i,] = ( x[i,] - mean(x[i,]) )  / sd(x[i,])
    }
    return(x)
  }

  corpus_norm <- ZTransf(corpus_lexical_table)
  
  head(corpus_norm)

  # Check na and infinite values : 
  
  a <- is.infinite(corpus_norm)
  b <- which(a == TRUE)

  if(length(b) > 0){
    corpus_norm <- corpus_norm[-b,]
  }
  
  c <- is.na(corpus_norm)
  d <- which(c == TRUE)

  if(length(d) > 0){
    corpus_norm <- corpus_norm[-d,]
  }
  
  # And now PCA : 
  
  corpus_PCA <- prcomp(t(corpus_norm), scale. = FALSE)
  
  # Impression des composants
  
  fviz_eig(corpus_PCA)
  
  # Plot !
  
  fviz_pca_ind(corpus_PCA,
               col.ind = "coord", # Colorer par le cos2
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = FALSE, 
  )
  
}

motifs_acp(path = "~/Dropbox/2020-2021/Motifs/", csv = "corpus_motifs_grams.csv", 
           freq_filter = 1)

