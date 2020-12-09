### Stage - Script Dominique Legallois ### 
### Analyse de la densité des motifs ###

## Analyse de densité des motifs une oeuvre : ##
path = "~/Dropbox/2019-2020/Stage/Test/"
csv = "Corpus_motifs_UDPipe.csv"
filtre = "13_germinal.txt"

motif1 = "NC à le NC de"
motif2 = "NC de le NC de"
motif3 = "le NC et le NC"
motif4 = "le ADJ NC de le"
motif5 = "à le NC ce être"
bd = 4000
titre_graphique = "Densité sur cinq motifs - Germinal"

motifs_densite <- function(path = "~/Dropbox/2019-2020/Stage/Test_Regex_R/", csv = "Corpus_motifs_UDPipe.csv", 
                           filtre = "13_germinal.txt", motif1 = "NC à le NC de", motif2 = "NC de le NC de",
                           motif3 = "le NC et le NC", motif4 = "le ADJ NC de le", motif5 = "à le NC ce être",
                           bd = 4000, titre_graphique = "Densité sur cinq motifs - Germinal"){
  
  require("dplyr")
  require("reshape2")
  require("readr")
  require("tidytext")
  require("tidyverse")
  require("ggplot2")
  require("ggridges")
  
  # Lecture des données :
  
  setwd(path)
  corpus_spec <- fread(csv)
  
  ## Retrait des cases vides :
  
  corpus_spec <- corpus_spec[complete.cases(corpus_spec),]
  
  ## Mise sous la forme tidy :
  
  # Vérification okazou :
  names(corpus_spec) <- c("mots", "motifs", "Oeuvre")
  
  # Filtre d'une oeuvre
  
  corpus_spec <- corpus_spec %>%
    filter(Oeuvre == filtre)
  
  ## Fivegrams :
  corpus_spec_punct <- corpus_spec  %>%
    mutate(next_word = lead(motifs),
           next_word2 = lead(motifs, 2),
           next_word3 = lead(motifs, 3),
           next_word4 = lead(motifs, 4)) %>%
    filter(!is.na(next_word), !is.na(next_word2)) %>%
    mutate(ngrammotif = paste(motifs, next_word, next_word2, next_word3, next_word4))
  
  # Sélection des colonnes motifs ngram et Oeuvre :
  corpus_spec_punct <- corpus_spec_punct[,c("mots", "ngrammotif")]
  
  # Extraction des motifs pertinents :
  
  corpus_spec_punct$m1 <- corpus_spec_punct$ngrammotif == "NC à le NC de"
  corpus_spec_punct$m2 <- corpus_spec_punct$ngrammotif == "NC de le NC de"
  corpus_spec_punct$m3 <- corpus_spec_punct$ngrammotif == "le NC et le NC"
  corpus_spec_punct$m4 <- corpus_spec_punct$ngrammotif == "le ADJ NC de le"
  corpus_spec_punct$m5 <- corpus_spec_punct$ngrammotif == "à le NC ce être"
  
  # Renommer les motifs :
  
  # Transformation des TRUE en rownumber :
  # == Transformer les TRUE en la valeur de l'index correspondante...
  
  true_to_rownb1 = which(corpus_spec_punct$m1 == TRUE)
  corpus_spec_punct$m1[corpus_spec_punct$m1 == TRUE] <- true_to_rownb1
  
  true_to_rownb2 = which(corpus_spec_punct$m2 == TRUE)
  corpus_spec_punct$m2[corpus_spec_punct$m2 == TRUE] <- true_to_rownb2
  
  true_to_rownb3 = which(corpus_spec_punct$m3 == TRUE)
  corpus_spec_punct$m3[corpus_spec_punct$m3 == TRUE] <- true_to_rownb3
  
  true_to_rownb4 = which(corpus_spec_punct$m4 == TRUE)
  corpus_spec_punct$m4[corpus_spec_punct$m4 == TRUE] <- true_to_rownb4
  
  true_to_rownb5 = which(corpus_spec_punct$m5 == TRUE)
  corpus_spec_punct$m5[corpus_spec_punct$m5 == TRUE] <- true_to_rownb5
  
  # Transformer les FALSE en NA :
  
  corpus_spec_punct$m1[corpus_spec_punct$m1 == FALSE] <- 0
  corpus_spec_punct$m2[corpus_spec_punct$m2 == FALSE] <- 0
  corpus_spec_punct$m3[corpus_spec_punct$m3 == FALSE] <- 0
  corpus_spec_punct$m4[corpus_spec_punct$m4 == FALSE] <- 0
  corpus_spec_punct$m5[corpus_spec_punct$m5 == FALSE] <- 0
  
  # Retrait colonne mots :
  
  corpus_spec_punct <- corpus_spec_punct[-1]
  
  # Renommer les colonnes pour que les motifs soient affichés dans le graphique
  
  names(corpus_spec_punct) <- c("ngrammotifs", as.character(motif1), as.character(motif2), as.character(motif3),
                                as.character(motif4), as.character(motif5))
  
  # Transformation des données :
  
  corpus_melt <- melt(corpus_spec_punct, id.var = "ngrammotifs")
  
  names(corpus_melt) <- c("ngrammotifs", "motifs", "value")
  
  # Transformation des 0 en NA
  
  corpus_melt$value[corpus_melt$value == 0] <- NA
  
  # Ajout d'une colonne index pour x : 1:n où n est le nb de mots dans l'oeuvre :
  # Jouer avec le paramètre bandwith pour faire varier les courbes de densité et mettre à 
  # la bonne échelle.
  
  ggplot(corpus_melt, aes(x = `value`, y = `motifs`, fill = `motifs`)) +
    stat_density_ridges(bandwidth = bd, na.rm = T) +
    scale_fill_viridis_d() +
    labs(title = titre_graphique) +
    theme_bw()
  
}


motifs_densite(path = "~/Dropbox/2019-2020/Stage/Test/", csv = "Corpus_motifs_UDPipe.csv", 
               filtre = "13_germinal.txt", motif1 = "NC à le NC de", motif2 = "NC de le NC de",
               motif3 = "le NC et le NC", motif4 = "le ADJ NC de le", motif5 = "à le NC ce être",
               bd = 4000, titre_graphique = "Densité sur cinq motifs - Germinal")

