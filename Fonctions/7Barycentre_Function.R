## Stage - Dominique Legallois ##

## Fonction pour le calcul barycentre : ##

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

# Entrée : Corpus_motifs_UDPipe.csv avec : mots || motifs || Oeuvre.

# Paramètres :

# path : chemin du fichier motifs csv (sortie du script de regex)
# csv : nom du fichier motifs.

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

path = "~/Dropbox/2020-2021/Corpus-test-motifs/"
csv = "Corpus_motifs_UDPipe.csv"

barycentre <- function(path = "~/Dropbox/2019-2020/Stage/Test_Regex_R/", csv = "Corpus_motifs_UDPipe.csv"){
  
  require("dplyr")
  require("readr")
  require("data.table")
  
  setwd(path)
  corpus_spec <- fread(csv, encoding = "UTF-8")
  
  corpus_spec <- as_tibble(corpus_spec) %>%
    group_by(Oeuvre)
  
  corpus <- corpus_spec
  
  # Vérification okazou :
  names(corpus) <- c("mots", "motifs", "Oeuvre")
  
  ## Retrait des cases vides :
  corpus <- corpus[complete.cases(corpus),]
  
  ## Fivegrams :
  corpus_punct <- corpus  %>%
    mutate(next_word = lead(motifs),
           next_word2 = lead(motifs, 2),
           next_word3 = lead(motifs, 3),
           next_word4 = lead(motifs, 4)) %>%
    filter(!is.na(next_word), !is.na(next_word2), !is.na(next_word3), !is.na(next_word4)) %>%
    mutate(ngrammotif = paste(motifs, next_word, next_word2, next_word3, next_word4))
  
  # Sélection des colonnes motifs ngram et Oeuvre :
  corpus_punct <- corpus_punct[,c("ngrammotif", "Oeuvre")]
  
  names(corpus_punct) = c("motifs", "Oeuvre")
  
  # il faudrait tout d'abord pouvoir numéroter les périodes par un indice augmentant de +1 à chaque changement de période.
  # La formule utilisée pour calculer le barycentre est alors : B = (Somme pour tout les i de 
  # ( Nombre d'occurences  à la période i * indice de la période i)) divisée par (Nombre total d'occurences de la forme)
  
  # Ajout d'une colonne index pour numéroter les périodes :
  # 1 période = 1 oeuvre :
  
  corpus_punct$index <- cumsum(!duplicated(corpus_punct$Oeuvre))

  # Correction d'un léger bug sur la nature de l'objet.
  # int => num
  
  corpus_punct$index <- as.numeric(corpus_punct$index)

  # str(corpus_punct)
  
  corpus_punct_n <- corpus_punct %>% 
    dplyr::count(motifs, index, sort = T)
  
  corpus_punct_total <- corpus_punct
  
  corpus_punct_total <- corpus_punct_total %>%
    ungroup(corpus_punct_total) %>%
    count(motifs, sort = T)
  
  names(corpus_punct_total) <- c("motifs", "n_total")
  
  corpus_baryc <- inner_join(corpus_punct_n, corpus_punct_total) ## TODO Ajouter cette fonctionnalité innerjoin aux retours aux textes. ##
  ## Colonnes de fréquences relative et absolues. ## 
  
  # Somme du nombre d'occ * indice / Nombre total d'occurrence de la forme.
  
  corpus_baryc <- corpus_baryc %>%
    ungroup(corpus_baryc) %>%
    mutate(barycentre = n * index / n_total)
  
  # Barycentres qui vont de 0 à 3 :
  # 3 : le motif représente toutes les occurrences totales : correspond donc à un motif qui n'est présent que dans une oeuvre.
  # 1.5 : le motif représente la moitié des occurrences totales : il est donc présent deux fois plus que dans le reste du corpus.
  # 0,1 : le motif est quasi-absent de l'oeuvre, mais très présent dans d'autres.
  
  round_df <- function(x, digits) {
    # round all numeric variables
    # x: data frame 
    # digits: number of digits to round
    numeric_columns <- sapply(x, mode) == 'numeric'
    x[numeric_columns] <-  round(x[numeric_columns], digits)
    x
  }
  
  baryc_arrondi <- round_df(corpus_baryc$barycentre, 3)
  
  # Changement des valeurs dans la dataframe :
  
  corpus_baryc$barycentre <- baryc_arrondi
  
  # Ordonnancement : 
  
  corpus_baryc <- corpus_baryc[order(-corpus_baryc$n_total),]
  
  # Transformation en pourcentage :
  
  corpus_barycentre_pourcentage <- corpus_baryc %>%
    mutate(pourcentage = n / n_total * 100)
  
  poucentage_arrondi <- round_df(corpus_barycentre_pourcentage$pourcentage, 2)
  
  # Changement des valeurs dans la dataframe :
  
  corpus_barycentre_pourcentage$pourcentage <- poucentage_arrondi
  
  toprint<-as.numeric((readline("Sauvegarder les résultats en csv, 'Barycentre_motifs.csv', tapez 1 et enter \n dans une variable R 'corpus_barycentre', tapez 2")))
  if(toprint==1){
    write.csv(corpus_barycentre_pourcentage, "Barycentre_motifs.csv", fileEncoding = "UTF-8")
  }
  if(toprint==2){
    corpus_barycentre <<- corpus_barycentre_pourcentage
  } 
}


barycentre(path = "~/Dropbox/2019-2020/Stage/Test_Regex_R/", csv = "Corpus_motifs_UDPipe.csv")






