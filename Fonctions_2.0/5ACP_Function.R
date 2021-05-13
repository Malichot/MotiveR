## Stage - Legallois ##
## Fonction AFC ## 

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

# path = "~/Dropbox/2019-2020/Stage/Corpus_Retour_au_texte/"
# csv = "Corpus_motifs.csv" (sortie du script de regex)
# une_oeuvre = "Rigodon.cnr" (si choix d'affichage d'une seule oeuvre, entrez le nom qui apparaît dans la colonne Oeuvre)
# nmotifs = 30 (nombre de motifs à afficher)
# nombre_dimensions = 5 (number of dimensions kept in the results)

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

path = "~/Dropbox/2020-2021/Corpus-test-motifs/"
csv = "Corpus_motifs_UDPipe.csv"
nombre_oeuvres = 2
nmotifs = 30
nombre_dimensions = 5
nb_grams = 5

motifs_afc <- function(path = "~/Dropbox/2019-2020/Stage/Test/", csv = "UDPipe_corpus_complet.csv", 
                       nombre_oeuvres = 4, nb_grams = 5, nmotifs = 30, nombre_dimensions = 5, 
                       une_oeuvre = "Rigodon.cnr"){
  
  # Réf : http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-dans-r-guide-pratique/73-acp-analyse-en-composantes-principales-avec-r-l-essentiel/
  
  # Librairies : 
  
  require("tidyverse")
  require("tidytext")
  require("dplyr")
  require("slider")
  require("FactoMineR")
  require("factoextra")
  require("ggplot2")
  require("ggrepel")
  require("ca")
  require("data.table")
  require("tidyr")
  
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
    mutate(ngrammotif = slide_chr(motifs, paste, collapse = " ", .after = nb_grams-1)) # %>%
    # head(-nb_grams-1) : ne fonctionne pas : Value of SET_STRING_ELT() must be a 'CHARSXP' not a 'character'

  # Transformation en tibble pour éviter l'erreur ?
  
  nb <- nb_grams-1
  corpus_spec_punct <- as_tibble(corpus_spec_punct) %>%
    head(-nb)
  
  # # Retrait des dernières lignes qui sont des NA puisque pas de motifs qui 
  # # suivent le dernier motif de la dataframe
  # 
  
  # nrow_df <- nrow(corpus_spec_punct)
  # clean_df <- nrow_df - nb
  # 
  # corpus_spec_punct <- corpus_spec_punct[1:clean_df,]
  
  
  # Retrait des NA éventuels
  
  a <- is.na(corpus_spec_punct$motifs)
  
  b <- which(a == TRUE)
  
  if(length(b) > 0){
    corpus_spec_punct <- corpus_spec_punct[-b,]
  }
  
    
  # Sélection des colonnes motifs ngram et Oeuvre :
  corpus_spec_punct <- corpus_spec_punct[,c("ngrammotif", "Oeuvre")]
  
  names(corpus_spec_punct) <- c("motifs", "Oeuvre")
  
  ## Dénombrement + filtrage éventuel des données : ex : n > 10
  corpus_spec_punct <- corpus_spec_punct %>%
    dplyr::count(Oeuvre, motifs, sort = TRUE)
  
  ## Reshaping the data : colonnes = corpus, lignes = mots et freq
  # Réf : https://stackoverflow.com/questions/19346066/r-re-arrange-dataframe-some-rows-to-columns
  
  corpus_lexical_table <- xtabs(n~motifs+Oeuvre, corpus_spec_punct)
  
  ## Ré-ordonnancement : 
  
  corpus_lexical_table <- corpus_lexical_table[order(-corpus_lexical_table[,1], corpus_lexical_table[,1]),]
  
  head(corpus_lexical_table)
  tail(corpus_lexical_table)
  
  # lignes = corpus ; colonnes = motifs.
  
  corpus_prep_PCA <- pivot_wider(data = corpus_spec_punct, names_from = motifs, 
                      values_from = n, id_cols = Oeuvre)
  
  head(corpus_prep_PCA)
  
  # Transformation en valeurs numériques :
  
  corpus_prep_PCA[2:ncol(corpus_prep_PCA)] <- lapply(corpus_prep_PCA[2:ncol(corpus_prep_PCA)], as.numeric)
  
  # Transformation en dataframe et rownames

  corpus_prep_PCA <- corpus_prep_PCA %>%
    remove_rownames() %>%
    column_to_rownames(var = 'Oeuvre')
  
  # head(corpus_prep_PCA)
  
  # Calcul des zscores : 
  
  ZTransf = function(x){
    for(i in 1:ncol(x)){
      x[,i] = (x[,i] - mean(x[,i]) )  / sd(x[,i])
    }
    return(x)
  }
  
  normalisations = function(x){
    # Z-transformation  
    x = ZTransf(x)
    
    # Vector length normalisation
    for(i in 1:nrow(x)){
      x[i,] = x[i,] / sqrt(sum(x[i,]^2))
    }
    return(x)
  }
  
  test <- normalisations(corpus_prep_PCA)
  
  head(test)
  
  # Retrait des variances égales à 0 : 
  # Thx : https://stackoverflow.com/questions/40315227/how-to-solve-prcomp-default-cannot-rescale-a-constant-zero-column-to-unit-var
  
  corpus_prep_PCA <- corpus_prep_PCA[ , which(apply(corpus_prep_PCA, 2, var) != 0)]
  
  head(corpus_prep_PCA)
  
  # Normalisation zscores des variables : 
  # différence entre la fréquence la moyenne de chaque obs, divisé par l'écart-type
  
  # corpus_PCA <- PCA(corpus_lexical_table, scale.unit = TRUE, ncp = 5, graph = TRUE)
  
  a <- ncol(corpus_prep_PCA) -1
  
  corpus_PCA <- prcomp(corpus_prep_PCA[,1:a], scale. = TRUE)
  
  fviz_eig(corpus_PCA)
  
  fviz_pca_ind(corpus_PCA,
               col.ind = "cos2", # Colorer par le cos2
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE     
  )
  
  
  fviz_pca_var(corpus_PCA,
               col.var = "contrib", 
               gradient.cols = c("#00AFBB", "#E7B800"),
               repel = TRUE     
  )
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

