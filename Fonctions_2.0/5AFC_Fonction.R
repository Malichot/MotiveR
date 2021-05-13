## Stage - Legallois ##
## Fonction AFC ## 

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

# path = "~/Dropbox/2019-2020/Stage/Corpus_Retour_au_texte/"
# csv = "Corpus_motifs.csv" (sortie du script de regex)
# une_oeuvre = "Rigodon.cnr" (si choix d'affichage d'une seule oeuvre, entrez le nom qui apparaît dans la colonne Oeuvre)
# nmotifs = 30 (nombre de motifs à afficher)
# nombre_dimensions = 5 (number of dimensions kept in the results)

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

path = "~/Dropbox/2019-2020/Stage/Test/"
csv = "Corpus_motifs_UDPipe.csv"
nombre_oeuvres = 2
nmotifs = 30
nombre_dimensions = 5

motifs_afc <- function(path = "~/Dropbox/2019-2020/Stage/Test/", csv = "UDPipe_corpus_complet.csv", 
                       nombre_oeuvres = 4, nb_grams = 5, nmotifs = 30, nombre_dimensions = 5, 
                       une_oeuvre = "Rigodon.cnr"){
  
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
  corpus_spec <- fread(csv, encoding = "UTF-8")
  
  ## Retrait des cases vides :
  
  corpus_spec <- corpus_spec[complete.cases(corpus_spec),]
  
  ## Mise sous la forme tidy :
  
  # Vérification okazou :
  names(corpus_spec) <- c("mots", "motifs", "Oeuvre")
  
  ## Retrait des cases vides :
  corpus_spec <- corpus_spec[complete.cases(corpus_spec),]
  
  ## Fivegrams :
  # corpus_spec_punct <- corpus_spec  %>%
  #   mutate(next_word = lead(motifs),
  #          next_word2 = lead(motifs, 2),
  #          next_word3 = lead(motifs, 3),
  #          next_word4 = lead(motifs, 4)) %>%
  #   filter(!is.na(next_word), !is.na(next_word2)) %>%
  #   mutate(ngrammotif = paste(motifs, next_word, next_word2, next_word3, next_word4))
  
  # Nouvelle fonction n-grams pour choix du gram :
  
  # Creating 5-grams means setting .after to 4 and removing last 4 rows
  # library : slider
  corpus_spec_punct <- corpus_spec %>%
    mutate(ngrammotif = slide_chr(motifs, paste, collapse = " ", .after = nb_grams-1))
  
  # Sélection des colonnes motifs ngram et Oeuvre :
  corpus_spec_punct <- corpus_spec_punct[,c("ngrammotif", "Oeuvre")]
  
  names(corpus_spec_punct) <- c("motifs", "Oeuvre")
  
  ## Dénombrement + filtrage éventuel des données : ex : n > 10
  corpus_spec_punct <- corpus_spec_punct %>%
    dplyr::count(Oeuvre, motifs, sort = TRUE)
  
  ## Ajout d'une colonne total words pour normaliser la fréquence (fréquence relative) :
  
  total_words <- corpus_spec_punct %>%
    group_by(Oeuvre) %>%
    dplyr::summarize(total = sum(n))
  
  corpus_words_ngrams <- left_join(corpus_spec_punct, total_words, by = "Oeuvre") 
  
  ## Calcul de la fréquence relative :
  
  corpus_words_ngrams$rel_freq <- corpus_words_ngrams$n / corpus_words_ngrams$total
  
  # Ordonnancement par fréquences relatives :
  corpus_words_ngrams <- corpus_words_ngrams[order(corpus_words_ngrams$rel_freq, decreasing = T),] 
  
  ## Reshaping the data : colonnes = corpus, lignes = mots et freq
  # Réf : https://stackoverflow.com/questions/19346066/r-re-arrange-dataframe-some-rows-to-columns
  
  corpus_lexical_table <- xtabs(rel_freq~motifs+Oeuvre, corpus_words_ngrams)
  
  ## Ré-ordonnancement : 
  
  corpus_lexical_table <- corpus_lexical_table[order(-corpus_lexical_table[,1], corpus_lexical_table[,1]),]
  
  head(corpus_lexical_table)
  tail(corpus_lexical_table)
  
  
  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  
  
  # Retrait des lignes contenant des ngrams qui ne sont pas dans tous les textes :
  # Cela veut dire : toutes les valeurs de ngrams qui sont uniques (qui contienne 0)
  # Renversement de la dataframe avec variable = corpus
  # rows = motifs
  # Retrait des motifs où une valeur = 0.
  
  ## Nouvelle matrice nettoyée : 
  
  row_substract <- apply(corpus_lexical_table, 1, function(row) all(row !=0 ))
  
  ## Subset :
  
  corpus_clean <- corpus_lexical_table[row_substract,]
  corpus_clean <- as.matrix(corpus_clean)
  head(corpus_clean)
  
  ## Visualisation : 
  
  
  maCA <- CA(corpus_clean, ncp = nombre_dimensions, row.sup = NULL, col.sup = NULL, 
             quanti.sup=NULL, quali.sup = NULL, graph = T, 
             axes = c(1,2), row.w = NULL, excl=NULL)
  
  
  
  # fviz_ca_biplot(maCA, title = "Analyse Factorielle des Correspondances")
  
  plot_ca <- fviz_ca_biplot(maCA, map ="rowprincipal", repel = T, select.row = list(contrib = nmotifs),
                            title = "Analyse Factorielle des Correspondances")
  
  # Avec gradient de couleur en fonction des coordonnées :
  
  plot_grad <- fviz_ca(maCA, map ="rowprincipal", repel = T, select.row = list(contrib = nmotifs), 
                       col.row = "coord", title = "Analyse Factorielle des Correspondances")
  
  # Une oeuvre particulière : 
  
  une_ca <- fviz_ca_biplot(maCA, map ="rowprincipal", repel = T, select.row = list(contrib = nmotifs), 
                           select.col = list(name = une_oeuvre), title = "Analyse Factorielle des Correspondances")
  
  visualisation <- as.numeric(readline("Visualisation, tapez 1 et enter \n Avec gradient de couleurs, tapez 2 \n Une oeuvre particulière, vérifiez que vous l'avez entrée dans le paramètre une_oeuvre et tapez 3"))
  
  if(visualisation == 1){
    return(plot_ca)
    
  }
  
  if(visualisation == 2){
    return(plot_grad)
  }
  
  if(visualisation == 3){
    return(une_ca)
  }
  
  else{
    print("Votre choix ne correspond pas aux critères ternaires proposés...!")
  }
  
}

motifs_afc(path = "~/Dropbox/2019-2020/Stage/Test/", csv = "UDPipe_corpus_complet.csv", 
           nombre_oeuvres = 4, nb_grams = 5, nmotifs = 30, nombre_dimensions = 5, 
           une_oeuvre = "Rigodon.cnr")

