### Stage - Script Dominique Legallois ### 
### Analyse de la densité des motifs ###

## Analyse de densité des motifs une oeuvre : ##

path = "~/Dropbox/2020-2021/Corpus-test-motifs/" 
csv = "Corpus_motifs_UDPipe.csv" 
nb_grams = 5
filtre = "Les_Beaux_Draps.txt" 
motif1 = "le NC , de le"
motif2 = "NC de le NC de"
motif3 = "le NC et le NC" 
motif4 = "le ADJ NC de le" 
motif5 = "NC à le NC de" 
bd = 4000
titre_graphique = "Densité sur cinq motifs - BD"

motifs_densite <- function(path = "~/Dropbox/2019-2020/Stage/Test_Regex_R/", csv = "Corpus_motifs_UDPipe.csv", nb_grams = 5,
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
  require("data.table")
  require("slider")
  
  # Lecture des données :
  
  setwd(path)
  corpus_spec <- fread(csv, encoding = "UTF-8")
  
  ## Retrait des cases vides :
  
  corpus_spec <- corpus_spec[complete.cases(corpus_spec),]
  
  ## Mise sous la forme tidy :
  
  # Vérification okazou :
  names(corpus_spec) <- c("mots", "motifs", "Oeuvre")
  
  # Filtre d'une oeuvre
  
  corpus_spec <- corpus_spec %>%
    filter(Oeuvre == filtre)
  
  ## Fivegrams :
  # corpus_spec_punct <- corpus_spec  %>%
  #   mutate(next_word = lead(motifs),
  #          next_word2 = lead(motifs, 2),
  #          next_word3 = lead(motifs, 3),
  #          next_word4 = lead(motifs, 4)) %>%
  #   filter(!is.na(next_word), !is.na(next_word2), !is.na(next_word3), !is.na(next_word4)) %>%
  #   mutate(ngrammotif = paste(motifs, next_word, next_word2, next_word3, next_word4))
  
  # Nouvelle fonction n-grams pour choix du gram :
  
  # Creating 5-grams means setting .after to 4 and removing last 4 rows
  # library : slider
  corpus_spec_punct <- corpus_spec %>%
    mutate(ngrammotif = slide_chr(motifs, paste, collapse = " ", .after = nb_grams-1))
  # head(-nb_grams-1) : ne fonctionne pas : Value of SET_STRING_ELT() must be a 'CHARSXP' not a 'character'
  
  # Transformation en tibble pour éviter l'erreur ?
  
  nb <- nb_grams-1
  corpus_spec_punct <- as_tibble(corpus_spec_punct) %>%
    head(-nb)
  
  # Sélection des colonnes motifs ngram et Oeuvre :
  corpus_spec_punct <- corpus_spec_punct[,c("mots", "ngrammotif")]
  
  # Extraction des motifs pertinents :
  
  corpus_spec_punct$m1 <- corpus_spec_punct$ngrammotif == motif1
  corpus_spec_punct$m2 <- corpus_spec_punct$ngrammotif == motif2
  corpus_spec_punct$m3 <- corpus_spec_punct$ngrammotif == motif3
  corpus_spec_punct$m4 <- corpus_spec_punct$ngrammotif == motif4
  corpus_spec_punct$m5 <- corpus_spec_punct$ngrammotif == motif5
  
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
    scale_fill_brewer() +
    labs(title = titre_graphique) +
    theme_bw()
  
}


motifs_densite(path = "~/Dropbox/2019-2020/Stage/Test_Regex_R/", csv = "Corpus_motifs_UDPipe.csv", nb_grams = 5,
               filtre = "13_germinal.txt", motif1 = "NC à le NC de", motif2 = "NC de le NC de",
               motif3 = "le NC et le NC", motif4 = "le ADJ NC de le", motif5 = "à le NC ce être",
               bd = 4000, titre_graphique = "Densité sur cinq motifs - Germinal")


