#' Titre : Scripts motifs - Densité
#' Auteurs : Dominique Legallois, Antoine de Sacy
#' Date: 6 octobre 2021.

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

## Analyse de densité des motifs une oeuvre : ##

motifs_densite <- function(path = "~/Dropbox/2020-2021/Motifs/", 
                           csv = "corpus_motifs_grams.csv", 
                           filtre = "Flaubert-Bovary.txt-ut8.txt", 
                           motif1 = "le NC de le NC",
                           motif2 = "NC de le NC ,",
                           motif3 = "le NC de DETPOSS NC",
                           motif4 = "à le NC de le",
                           motif5 = "NC de le NC .",
                           bd = 4000,
                           titre_graphique = "Densité sur cinq motifs - Madame Bovary"){
  
  require("dplyr")
  require("reshape2")
  require("readr")
  require("tidytext")
  require("tidyverse")
  require("ggplot2")
  require("ggridges")
  require("data.table")

  # Lecture des données :
  
  setwd(path)
  corpus <- fread(csv, encoding = "UTF-8", header = TRUE, stringsAsFactors = FALSE)
  
  # Vérification okazou (pb index) :
  corpus <- corpus[,c("mots", "motifs", "Oeuvre")]
  
  ## Retrait des cases vides :
  corpus <- corpus[complete.cases(corpus),]
  
  # Filtre d'une oeuvre
  
  corpus <- corpus %>%
    filter(Oeuvre == filtre)

  # Changement de nom de colonnes pour coller au script : 
  
  corpus_dens <- corpus
  
  names(corpus_dens) <- c('mots', 'ngrammotif', 'Oeuvre')
  
  # Sélection des colonnes motifs ngram et Oeuvre :
  corpus_dens <- corpus_dens[,c("mots", "ngrammotif")]
  
  # Extraction des motifs pertinents :
  
  corpus_dens$m1 <- corpus_dens$ngrammotif == motif1
  corpus_dens$m2 <- corpus_dens$ngrammotif == motif2
  corpus_dens$m3 <- corpus_dens$ngrammotif == motif3
  corpus_dens$m4 <- corpus_dens$ngrammotif == motif4
  corpus_dens$m5 <- corpus_dens$ngrammotif == motif5
  
  # Renommer les motifs :
  
  # Transformation des TRUE en rownumber :
  # == Transformer les TRUE en la valeur de l'index correspondante...
  
  true_to_rownb1 = which(corpus_dens$m1 == TRUE)
  corpus_dens$m1[corpus_dens$m1 == TRUE] <- true_to_rownb1
  
  true_to_rownb2 = which(corpus_dens$m2 == TRUE)
  corpus_dens$m2[corpus_dens$m2 == TRUE] <- true_to_rownb2
  
  true_to_rownb3 = which(corpus_dens$m3 == TRUE)
  corpus_dens$m3[corpus_dens$m3 == TRUE] <- true_to_rownb3
  
  true_to_rownb4 = which(corpus_dens$m4 == TRUE)
  corpus_dens$m4[corpus_dens$m4 == TRUE] <- true_to_rownb4
  
  true_to_rownb5 = which(corpus_dens$m5 == TRUE)
  corpus_dens$m5[corpus_dens$m5 == TRUE] <- true_to_rownb5
  
  # Transformer les FALSE en NA :
  
  corpus_dens$m1[corpus_dens$m1 == FALSE] <- 0
  corpus_dens$m2[corpus_dens$m2 == FALSE] <- 0
  corpus_dens$m3[corpus_dens$m3 == FALSE] <- 0
  corpus_dens$m4[corpus_dens$m4 == FALSE] <- 0
  corpus_dens$m5[corpus_dens$m5 == FALSE] <- 0
  
  # Retrait colonne mots :
  
  corpus_dens <- corpus_dens[,-1]
  
  # Renommer les colonnes pour que les motifs soient affichés dans le graphique
  
  names(corpus_dens) <- c("ngrammotifs", as.character(motif1), as.character(motif2), as.character(motif3),
                                as.character(motif4), as.character(motif5))
  
  # Transformation des données :
  
  corpus_melt <- melt(corpus_dens, id.var = "ngrammotifs")
  
  names(corpus_melt) <- c("ngrammotifs", "motifs", "value")
  
  # Transformation des 0 en NA
  
  corpus_melt$value[corpus_melt$value == 0] <- NA
  
  # Ajout d'une colonne index pour x : 1:n où n est le nb de mots dans l'oeuvre :
  # Jouer avec le paramètre bandwith pour faire varier les courbes de densité et mettre à 
  # la bonne échelle.
  
  ggplot(corpus_melt, aes(x = `value`, y = `motifs`, fill = `motifs`)) +
    stat_density_ridges(bandwidth = bd, na.rm = T) +
    scale_fill_brewer() +
    labs(title = titre_graphique) +
    theme_bw()
  
}


motifs_densite(path = "~/Desktop/Motifs/", 
               csv = "corpus_motifs_grams.csv", 
               filtre = "Flaubert-Bovary.txt-ut8.txt", 
               motif1 = "le NC de le NC",
               motif2 = "NC de le NC ,",
               motif3 = "le NC de DETPOSS NC",
               motif4 = "à le NC de le",
               motif5 = "NC de le NC .",
               bd = 4000,
               titre_graphique = "Densité sur cinq motifs - Madame Bovary")


