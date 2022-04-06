#' Titre : Scripts motifs - Fonctions
#' Auteurs : Dominique Legallois, Antoine de Sacy
#' Date: 6 octobre 2021.

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

## ## ## ## ## ## ## ## ## ## ## ##  LISTE DES FONCTIONS ## ## ## ## ## ## ## ## ## ## ## ## ##

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

# Fonction Nuage de mots (màj : 15 mai 2021) : 

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

# Fonction Histogramme (màj : 15 mai 2021) : 

motifs_histograms <- function(path = "~/Dropbox/2020-2021/Motifs/", 
                         csv = "corpus_motifs_grams.csv", nmots = 25){
  
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
  corpus_grams <- fread(csv, encoding = "UTF-8", header = TRUE, stringsAsFactors = FALSE)
  
  # Vérification okazou :
  corpus_grams <- corpus_grams[,c("mots", "motifs", "Oeuvre")]
  
  ## Retrait des cases vides :
  corpus_grams <- corpus_grams[complete.cases(corpus_grams),]

  ## Dénombrement + filtrage éventuel des données : ex : n > 10
  corpus_grams <- corpus_grams %>%
    count(motifs, Oeuvre, sort = TRUE)
  
  ## Ajout d'une colonne total words pour normaliser la fréquence (fréquence relative) :
  
  total_words <- corpus_grams %>%
    group_by(Oeuvre) %>%
    summarize(total = sum(n))
  
  corpus_words_ngrams <- left_join(corpus_grams, total_words, by = "Oeuvre") 
  
  ## Calcul de la fréquence relative :
  
  corpus_words_ngrams$rel_freq <- corpus_words_ngrams$n / corpus_words_ngrams$total
  
  # Ordonnancement par fréquences relatives :
  corpus_words_ngrams <- corpus_words_ngrams[order(corpus_words_ngrams$rel_freq, decreasing = T),] 
  
  # Visualisation en histogrammes : 
  
  ## Pour visualisation par fréquences décroissantes :
  
  #Turn your 'treatment' column into a character vector
  corpus_words_ngrams$motifs <- as.character(corpus_words_ngrams$motifs)
  #Then turn it back into a factor with the levels in the correct order
  corpus_words_ngrams$motifs <- factor(corpus_words_ngrams$motifs, levels=unique(corpus_words_ngrams$motifs))
  
  ## Fréquences absolues :
  
  plot_abs <- ggplot(data = corpus_words_ngrams[1:nmots,],
                           aes(x = motifs, y = n, fill = Oeuvre)) +
    geom_histogram(stat = "identity", position = "dodge", show.legend = T) + 
    theme_minimal() +
    theme(legend.position="top")
  
  plot_abs <- plot_abs + theme(axis.text.x= element_text(angle = 50, hjust = 1))
  
  ## Fréquences relatives
  
  # Réordonnancement de la df par fréquences relatives :
  
  df_freq_rel <- corpus_words_ngrams %>%
    arrange(-rel_freq)
  
  # Visualisation :
  
  plot_freq <- ggplot(data = df_freq_rel[1:nmots,],
                           aes(x = motifs, y = rel_freq, fill = Oeuvre)) +
    geom_histogram(stat = "identity", position = "dodge", show.legend = T) + 
    theme_minimal() +
    theme(legend.position="top")
  
  plot_freq <- plot_freq + theme(axis.text.x= element_text(angle = 50, hjust = 1))
  
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


# Fonction TF-IDF (màj : 15 mai 2021) :

tf_idf_motifs <- function(path = "~/Dropbox/2020-2021/Motifs/",
                          csv = "corpus_motifs_grams.csv", nombre_motifs = 20){
  
  ## Importation des librairies : 
  
  require("tidytext")
  require("tidyverse")
  require("dplyr")
  # require("slider")
  require("ggplot2")
  require("tidyr")
  require("data.table")
  
  # Lecture des données :
  
  setwd(path)
  corpus_grams <- fread(csv, encoding = "UTF-8", 
                       header = TRUE, stringsAsFactors = FALSE)
  
  # Vérification okazou :
  corpus_grams <- corpus_grams[,c("mots", "motifs", "Oeuvre")]
  
  ## Retrait des cases vides :
  
  corpus_grams <- corpus_grams[complete.cases(corpus_grams),]
  
  ## Dénombrement + filtrage éventuel des données : ex : n > 10
  corpus_words_ngrams <- corpus_grams %>%
    count(Oeuvre, motifs, sort = TRUE) %>%
    filter(n > 1)
  
  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ##  
  
  # TF-IDF : 
  
  ## Ajout d'une colonne total words pour normaliser la fréquence (fréquence relative) :
  
  total_words <- corpus_words_ngrams %>%
    group_by(Oeuvre) %>%
    summarize(total = sum(n))
  
  corpus_words_ngrams <- left_join(corpus_words_ngrams, total_words) 
  
  corpus_words_ngrams <- corpus_words_ngrams %>%
    bind_tf_idf(motifs, Oeuvre, n)
  
  corpus_words_ngrams %>%
    select(-total) %>%
    arrange(desc(tf_idf))
  
  tf_idf_export <- corpus_words_ngrams %>%
    select(-total) %>%
    arrange(desc(tf_idf))
  
  # Visualisation :
  
  tf_idf_grid <- corpus_words_ngrams %>%
    select(-total) %>%
    arrange(desc(tf_idf)) %>%
    mutate(motifs = factor(motifs, levels = rev(unique(motifs)))) %>% 
    group_by(Oeuvre) %>% 
    top_n(nombre_motifs) %>% # À moduler suivant les besoins : > x ; == x ; etc.
    ungroup %>%
    ggplot(aes(motifs, tf_idf, fill = Oeuvre)) +
    geom_col(show.legend = T) +
    labs(x = NULL, y = "TF-IDF") +
    facet_wrap(~Oeuvre, ncol = 2, scales = "free") +
    coord_flip() +
    theme_minimal()
  
  ## Autre visualisation : ##
  
  tf_idf_all <- corpus_words_ngrams %>% 
    select(-total) %>%
    arrange(desc(tf_idf)) %>%
    mutate(motifs = factor(motifs, levels = rev(unique(motifs)))) %>% 
    top_n(nombre_motifs) %>% # À moduler suivant les besoins : > x ; == x ; etc.
    ungroup() %>%
    mutate(word = reorder(motifs, tf_idf)) %>%
    ggplot(aes(motifs, tf_idf, fill = Oeuvre)) +
    geom_bar(stat = "identity") +
    ylab("TF-IDF") +
    coord_flip() +
    theme_minimal()
  
  plot_tfidf <- as.numeric(readline("Visualisation séparée, tapez 1 et enter \n Visualisation groupée, tapez 2 et enter \n Sauvergarde dans un csv, tapez 3"))
  
  if(plot_tfidf == 1){
    return(tf_idf_grid)
    
  }
  
  if(plot_tfidf == 2){
    return(tf_idf_all)
  }
  
  if(plot_tfidf == 3){
    write_csv(tf_idf_export, "Tf-idf.csv")
  }
  
  else{
    print("Votre choix ne correspond pas aux critères ternaires proposés...!")
  }

}

# Fonction Analyse en composante principale (màj : 28 mai 2021) :

motifs_acp <- function(path = "~/Dropbox/2020-2021/Motifs/", csv = "corpus_motifs_grams.csv", 
                       freq_filter = 1, n_obs = 50){
  
  # Librairies : 
  
  require("tidyverse")
  require("tidytext")
  require("dplyr")
  # require("slider")
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
  
  if(n_obs == "all"){
    corpus_PCA <- prcomp(corpus_norm[1:nrow(corpus_norm),], scale. = FALSE)
  }
  else {
    corpus_PCA <- prcomp(corpus_norm[1:n_obs,], scale. = FALSE)
  }

  
  # Impression des composants
  
  fviz_eig(corpus_PCA)
  
  # Plot observations !

  plot_obs <- fviz_pca_ind(corpus_PCA,
                           col.ind = "coord", # Colorer par le cos2
                           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                           repel = FALSE, 
  )
  
  # Plot variables ! 
  
  plot_var <- fviz_pca_var(corpus_PCA,
               col.var = "coord", 
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE     
  )
  
  # Plot variables + obs ! 
  
  plot_bis <- fviz_pca_biplot(corpus_PCA, repel = TRUE,
                  col.var = "#2E9FDF", 
                  col.ind = "#696969"  
  )
  
  msg <- as.numeric(readline("Plot variables, tapez 1 et enter \n , Plot motifs tapez 2 et enter \n Plot motifs + variables, tapez 3 et enter"))
  
  if(msg == 1){
    fviz_eig(corpus_PCA)
    return(plot_var)
  }
  if(msg == 2){
    fviz_eig(corpus_PCA)
    return(plot_obs)
  }
  if(msg == 3){
    fviz_eig(corpus_PCA)
    return(plot_bis)
  }
  
  else{
    print("Votre choix ne correspond pas aux critères ternaires proposés...!")
  }
  
}

# Fonction Calcul de spécificités (màj : 15 mai 2021) :

calcul_de_specificites <- function(path = "~/Dropbox/2020-2021/Motifs/",
                                   csv = "corpus_motifs_grams.csv",
                                   retrait_frequence_1 = TRUE){
  
  ## Librairies :
  require("dplyr")
  require("tidytext")
  require("tidyverse")
  require("ggplot2")
  require("tidyr")
  require("data.table")
  require("reshape2")
  
  ## Répertoire de travail :
  setwd(path)
  corpus_spec <- fread(csv, encoding = "UTF-8", 
                       header = TRUE, stringsAsFactors = FALSE)
  
  # Vérification okazou (pb index) :
  corpus_spec <- corpus_spec[,c("mots", "motifs", "Oeuvre")]
  
  ## Retrait des cases vides :
  corpus_spec <- corpus_spec[complete.cases(corpus_spec),]
  
  ## Dénombrement + filtrage éventuel des données : ex : n > 10
  corpus_spec <- corpus_spec %>%
    count(Oeuvre, motifs, sort = TRUE)
  
  ## Ajout d'une colonne total words pour normaliser la fréquence (fréquence relative) :
  
  total_words <- corpus_spec %>%
    group_by(Oeuvre) %>%
    summarize(total = sum(n))
  
  corpus_spec <- left_join(corpus_spec, total_words)
  
  ## Calcul de la fréquence relative :
  
  corpus_spec$rel_freq <- corpus_spec$n / corpus_spec$total
  
  corpus_words_ngrams_spec <- corpus_spec
  
  ## Reshaping the data : colonnes = corpus, lignes = mots et freq
  corpus_lexical_table <- xtabs(n~motifs+Oeuvre, corpus_words_ngrams_spec)
  
  ## Ré-ordonnancement : 
  corpus_lexical_table <- corpus_lexical_table[order(-corpus_lexical_table[,1], corpus_lexical_table[,1]),]
  
  # Retrait des lignes contenant des ngrams qui ne sont pas dans tous les textes :
  
  # Cela veut dire : toutes les valeurs de ngrams qui sont uniques (qui contienne 0)
  
  #row_substract <- apply(corpus_lexical_table, 1, function(row) all(row !=0 ))
  
  ## Subset :
  #corpus_clean <- corpus_lexical_table[row_substract,]
  #corpus_clean <- as.matrix(corpus_clean)
  
  corpus_clean <- corpus_lexical_table
  
  ### CALCUL SPÉCIFICITÉS : ###
  
  # lexicaltable = a matrix of nrow parts and ncol type where :
  # f : fréquence absolu dans un corpus 
  # t : nombre de mots du sub-corpus
  # F : nombre d'apparition du mots dans tout le corpus :
  # T : nombre total de mots dans le corpus
  
  `specificites` <-
    function(lexicaltable, types=NULL, parts=NULL) {
      spe <- specificites.probabilities(lexicaltable, types, parts);
      #dim(spe);
      spelog <- matrix(0, nrow=nrow(spe), ncol=ncol(spe));
      spelog[spe < 0.5] <- log10(spe[spe < 0.5]);
      spelog[spe > 0.5] <- abs(log10(1 - spe[spe > 0.5]));
      spelog[spe == 0.5] <- 0;
      spelog[is.infinite(spe)] <- 0;
      spelog <- round(spelog, digits=4);
      rownames(spelog) <- rownames(spe);
      colnames(spelog) <- colnames(spe);
      class(spelog) <- "specificites";
      attr(spelog, "frequency.table") <- lexicaltable;
      attr(spelog, "types") <- types;
      attr(spelog, "parts") <- parts;
      attr(spelog, "corpussize") <- attr(spe, "F");
      
      return(spelog);
    }
  
  # lexicaltable = a matrix of nrow parts and ncol type
  `specificites.probabilities` <-
    function(lexicaltable, types=NULL, parts=NULL) {
      
      #if (!is.numeric(lexicaltable)) stop("The lexical table must contain numeric values.");
      
      colMargin <- colSums(lexicaltable); # or "F" (the total frequency of all the types).
      rowMargin <- rowSums(lexicaltable); # or "T" (the size of the parts).
      F <- sum(colMargin);             # The grand total (number of tokens in the corpus).
      
      if (! is.null(types)) {      # Filter on tokens to be considered.
        if(is.character(types)) {  # convert the name of types given with "types" into row index numbers.
          if (is.null(rownames(lexicaltable))) {
            stop("The lexical table has no row names and the \"types\" argument is a character vector.");
          }
          if (! all(types %in% rownames(lexicaltable))) stop(paste(
            "Some requested types are not known in the lexical table: ",
            paste(types[! (types %in% rownames(lexicaltable))], collapse=" ")
          )
          ); 
        } else {
          if (any(types < 1)) stop("The row index must be greater than 0.");
          if (max(types) > nrow(lexicaltable)) stop("Row index must be smaller than the number of rows.");
        }
        lexicaltable <- lexicaltable[ , types, drop = FALSE];
        colMargin <- colMargin[types];
      }
      
      if (! is.null(parts)) {      # Filter on parts to be considered.
        if(is.character(parts)) {  # convert the name of parts given with "parts" into col index numbers.
          if (is.null(colnames(lexicaltable))) {
            stop("The lexical table has no col names and the \"parts\" argument is a character vector.");
          }
          if (! all(parts %in% colnames(lexicaltable))) stop(paste(
            "Some requested parts are not known in the lexical table: ",
            paste(parts[! (parts %in% colnames(lexicaltable))], collapse=" "))
          ); 
        } else {
          if (max(parts) > ncol(lexicaltable)) stop("Column index must be smaller than the number of cols.");
          if (any(parts < 1)) stop("The col index must be greater than 0.");
        }
        lexicaltable <- lexicaltable[parts, , drop=FALSE];
        rowMargin <- rowMargin[parts];
      }
      
      if (nrow(lexicaltable) == 0 | ncol(lexicaltable) == 0) {
        stop("The lexical table must contains at least one row and one column.");
      }
      
      specif <- matrix(0.0, nrow=nrow(lexicaltable), ncol=ncol(lexicaltable));
      
      for(i in 1:nrow(lexicaltable)) {    # We proceed the whole lexical table by row (i.e. by part).
        
        whiteDrawn <- lexicaltable[i,];  # The frequencies observed in this part for each type.
        white <- colMargin;     # The total frequencies in the corpus for each type.
        black <- F-white;       # The total complement frequency in the corpus for each type.
        drawn <- rowMargin[i];  # The total number of occurrences in the part.
        
        independance    <- (white * drawn) / F;         # The theoretic frequency of each type.
        specif_negative <- whiteDrawn <  independance;  # index of observed frequencies below the theoretic frequencies.
        specif_positive <- whiteDrawn >= independance;  # index of observed frequencies above the theoretic frequencies.
        
        specif[i, specif_negative] <- phyper (
          whiteDrawn[specif_negative], white[specif_negative], black[specif_negative], drawn
        );
        
        specif[i, specif_positive] <- phyper (
          whiteDrawn[specif_positive] - 1, white[specif_positive], black[specif_positive], drawn
        );
      }
      
      dimnames(specif) <- dimnames(lexicaltable);
      
      attr(specif, "F") <- F;
      return(specif);
    }
  
  calcul_spec <- specificites(corpus_clean)
  
  ##
  
  calcul_spec_test <- specificites.probabilities(corpus_clean)
  
  head(calcul_spec_test)
  
  #####
  
  calcul_spec <- as.data.frame.matrix(calcul_spec)
  
  # Transformation des lignes dans la variable motifs :
  
  calcul_spec <- setDT(calcul_spec, keep.rownames = "motifs")[]
  
  ## Ajout de la table de fréquences :
  
  colnames(corpus_words_ngrams_spec) <- c("Oeuvre", "motifs", "n", "total", "nrel")
  
  # Fusion des dataframes :
  calcul_spec_freq <- inner_join(corpus_words_ngrams_spec, calcul_spec)
  
  # Retrait éventuel des fréquences < 1 pour réduction de la taille du corpus : 
  
  if(retrait_frequence_1 == TRUE){
    
    calcul_spec_freq <- calcul_spec_freq %>%
      filter(n > 1)
    
  }
  
  toprint<-as.numeric((readline("Sauvegarder les résultats en csv, 'Corpus_motifs_specificites.csv', tapez 1 et enter\nSauvegarder les résulats avec fréquences 'Corpus_spec_freq' (pour retour au texte) tapez 2\nSavegarder les résultats dans une variable 'res', tapez 3")))
  if(toprint==1){
    write.csv(calcul_spec, "Corpus_motifs_specificites.csv", fileEncoding = "UTF-8")
  }
  if(toprint==2){
    write.csv(calcul_spec_freq, "Corpus_spec_freq.csv", fileEncoding = "UTF-8")
  }
  if(toprint==3){
    res <<- calcul_spec_freq
  }
  
}

# Fonction calcul de densité (màj : 15 mai 2021) :
  
motifs_densite <- function(path = "~/Dropbox/2020-2021/Motifs/", 
                           csv = "corpus_motifs_grams.csv", 
                           filtre = "Flaubert-Bovary.txt", 
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

# Fonction pour statistiques générales (màj : 15 mai 2021) :

stats_motifs <- function(path = "~/Dropbox/2020-2021/Motifs/", 
                         csv = "corpus_motifs_grams.csv"){
  
  ## Librairies :
  require("tidytext")
  require("tidyverse")
  require("ggplot2")
  require("tidyr")
  require("data.table")
  require("reshape2")
  
  ## Répertoire de travail :
  setwd(path)
  corpus_spec <- fread(csv, encoding = "UTF-8", 
                       header = TRUE, stringsAsFactors = FALSE)
  
  # Vérification okazou (pb index) :
  corpus_spec <- corpus_spec[,c("mots", "motifs", "Oeuvre")]
  
  ## Retrait des cases vides :
  corpus_spec <- corpus_spec[complete.cases(corpus_spec),]
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  ## ## ## ## ## ## ## ## ## ## ## ##  BARYCENTRES ## ## ## ## ## ## ## ## ## ## ## ## ## #
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  corpus_punct <- corpus_spec
  
  corpus_punct$index <- cumsum(!duplicated(corpus_punct$Oeuvre))
  
  # Correction d'un léger bug sur la nature de l'objet.
  # int => num
  
  corpus_punct$index <- as.numeric(corpus_punct$index)
  
  # str(corpus_punct)
  
  corpus_punct_n <- corpus_punct %>% 
    dplyr::count(motifs, index, sort = T)
  
  corpus_punct_total <- corpus_punct
  
  corpus_punct_total <- corpus_punct_total %>%
    dplyr::ungroup() %>%
    dplyr::count(motifs, sort = T)
  
  names(corpus_punct_total) <- c("motifs", "n_total")
  
  corpus_baryc <- inner_join(corpus_punct_n, corpus_punct_total)
  ## Colonnes de fréquences relative et absolues. ## 
  
  # Somme du nombre d'occ * indice / Nombre total d'occurrence de la forme.
  
  corpus_baryc <- corpus_baryc %>%
    ungroup() %>%
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
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  ## ## ## ## ## ## ## ## ## ## ## # SPÉCIFICITÉS # ## ## ## ## ## ## ## ## ## ## ## ## ##
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  ## Dénombrement + filtrage éventuel des données : ex : n > 10
  
  corpus_spec <- corpus_spec %>%
    count(Oeuvre, motifs, sort = TRUE)
  
  ## Ajout d'une colonne total words pour normaliser la fréquence (fréquence relative) :
  
  total_words <- corpus_spec %>%
    group_by(Oeuvre) %>%
    summarize(total = sum(n))
  
  corpus_spec <- left_join(corpus_spec, total_words)
  
  ## Calcul de la fréquence relative :
  
  corpus_spec$rel_freq <- corpus_spec$n / corpus_spec$total
  
  corpus_words_ngrams_spec <- corpus_spec
  
  ## Reshaping the data : colonnes = corpus, lignes = motifs et freq
  corpus_lexical_table <- xtabs(n~motifs+Oeuvre, corpus_words_ngrams_spec)
  
  ## Ré-ordonnancement : 
  corpus_lexical_table <- corpus_lexical_table[order(-corpus_lexical_table[,1], corpus_lexical_table[,1]),]
  
  # Retrait des lignes contenant des ngrams qui ne sont pas dans tous les textes :
  # Cela veut dire : toutes les valeurs de ngrams qui sont uniques (qui contienne 0)
  
  #row_substract <- apply(corpus_lexical_table, 1, function(row) all(row !=0 ))
  
  ## Subset :
  #corpus_clean <- corpus_lexical_table[row_substract,]
  #corpus_clean <- as.matrix(corpus_clean)
  
  corpus_clean <- corpus_lexical_table
  
  ### CALCUL SPÉCIFICITÉS : ###
  
  # lexicaltable = a matrix of nrow parts and ncol type where :
  # f : fréquence absolu dans un corpus 
  # t : nombre de mots du sub-corpus
  # F : nombre d'apparition du mots dans tout le corpus :
  # T : nombre total de mots dans le corpus
  
  `specificites` <-
    function(lexicaltable, types=NULL, parts=NULL) {
      spe <- specificites.probabilities(lexicaltable, types, parts);
      #dim(spe);
      spelog <- matrix(0, nrow=nrow(spe), ncol=ncol(spe));
      spelog[spe < 0.5] <- log10(spe[spe < 0.5]);
      spelog[spe > 0.5] <- abs(log10(1 - spe[spe > 0.5]));
      spelog[spe == 0.5] <- 0;
      spelog[is.infinite(spe)] <- 0;
      spelog <- round(spelog, digits=4);
      rownames(spelog) <- rownames(spe);
      colnames(spelog) <- colnames(spe);
      class(spelog) <- "specificites";
      attr(spelog, "frequency.table") <- lexicaltable;
      attr(spelog, "types") <- types;
      attr(spelog, "parts") <- parts;
      attr(spelog, "corpussize") <- attr(spe, "F");
      
      return(spelog);
    }
  
  # lexicaltable = a matrix of nrow parts and ncol type
  `specificites.probabilities` <-
    function(lexicaltable, types=NULL, parts=NULL) {
      
      #if (!is.numeric(lexicaltable)) stop("The lexical table must contain numeric values.");
      
      colMargin <- colSums(lexicaltable); # or "F" (the total frequency of all the types).
      rowMargin <- rowSums(lexicaltable); # or "T" (the size of the parts).
      F <- sum(colMargin);             # The grand total (number of tokens in the corpus).
      
      if (! is.null(types)) {      # Filter on tokens to be considered.
        if(is.character(types)) {  # convert the name of types given with "types" into row index numbers.
          if (is.null(rownames(lexicaltable))) {
            stop("The lexical table has no row names and the \"types\" argument is a character vector.");
          }
          if (! all(types %in% rownames(lexicaltable))) stop(paste(
            "Some requested types are not known in the lexical table: ",
            paste(types[! (types %in% rownames(lexicaltable))], collapse=" ")
          )
          ); 
        } else {
          if (any(types < 1)) stop("The row index must be greater than 0.");
          if (max(types) > nrow(lexicaltable)) stop("Row index must be smaller than the number of rows.");
        }
        lexicaltable <- lexicaltable[ , types, drop = FALSE];
        colMargin <- colMargin[types];
      }
      
      if (! is.null(parts)) {      # Filter on parts to be considered.
        if(is.character(parts)) {  # convert the name of parts given with "parts" into col index numbers.
          if (is.null(colnames(lexicaltable))) {
            stop("The lexical table has no col names and the \"parts\" argument is a character vector.");
          }
          if (! all(parts %in% colnames(lexicaltable))) stop(paste(
            "Some requested parts are not known in the lexical table: ",
            paste(parts[! (parts %in% colnames(lexicaltable))], collapse=" "))
          ); 
        } else {
          if (max(parts) > ncol(lexicaltable)) stop("Column index must be smaller than the number of cols.");
          if (any(parts < 1)) stop("The col index must be greater than 0.");
        }
        lexicaltable <- lexicaltable[parts, , drop=FALSE];
        rowMargin <- rowMargin[parts];
      }
      
      if (nrow(lexicaltable) == 0 | ncol(lexicaltable) == 0) {
        stop("The lexical table must contains at least one row and one column.");
      }
      
      specif <- matrix(0.0, nrow=nrow(lexicaltable), ncol=ncol(lexicaltable));
      
      for(i in 1:nrow(lexicaltable)) {    # We proceed the whole lexical table by row (i.e. by part).
        
        whiteDrawn <- lexicaltable[i,];  # The frequencies observed in this part for each type.
        white <- colMargin;     # The total frequencies in the corpus for each type.
        black <- F-white;       # The total complement frequency in the corpus for each type.
        drawn <- rowMargin[i];  # The total number of occurrences in the part.
        
        independance    <- (white * drawn) / F;         # The theoretic frequency of each type.
        specif_negative <- whiteDrawn <  independance;  # index of observed frequencies below the theoretic frequencies.
        specif_positive <- whiteDrawn >= independance;  # index of observed frequencies above the theoretic frequencies.
        
        specif[i, specif_negative] <- phyper (
          whiteDrawn[specif_negative], white[specif_negative], black[specif_negative], drawn
        );
        
        specif[i, specif_positive] <- phyper (
          whiteDrawn[specif_positive] - 1, white[specif_positive], black[specif_positive], drawn
        );
      }
      
      dimnames(specif) <- dimnames(lexicaltable);
      
      attr(specif, "F") <- F;
      return(specif);
    }
  
  calcul_spec <- specificites(corpus_clean)
  
  calcul_spec <- as.data.frame.matrix(calcul_spec)
  
  # Transformation des lignes dans la variable motifs :
  
  calcul_spec <- setDT(calcul_spec, keep.rownames = "motifs")[]
  
  ## Ajout de la table de fréquences :
  
  colnames(corpus_words_ngrams_spec) <- c("Oeuvre", "motifs", "n", "nb_total_mots", "n_rel")
  
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  ## ## ## ## ## ## ## ## ## ## FUSION DES DATAFRAMES ## ## ## ## ## ## ## ## ## ## ## ## #
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  
  # Fusion calcul de spécificités avec fréquences absolues, nombre total de mots dans le corpus, frequences relatives, specificité.
  
  calcul_spec_freq <- inner_join(corpus_words_ngrams_spec, calcul_spec)
  
  # Ajout des barycentres :
  
  corpus_final <- inner_join(calcul_spec_freq, corpus_barycentre_pourcentage)
  
  corpus_final <- subset(corpus_final, select=-c(index))
  
  
  
  
  toprint<-as.numeric((readline("Sauvegarder les résultats en csv, 'Motifs_statistisques.csv', tapez 1 et enter \n Sauvegarder les résultats dans une variable R corpus_final, tapez 2 et enter")))
  if(toprint==1){
    write.csv(corpus_final, "Motifs_statistisques.csv", fileEncoding = "UTF-8")
  }
  if(toprint==2){
    result_df_stats <<- corpus_final
  }
  
}


# Fonction de retour aux textes (màj : 15 mai 2021) : 

retour_texte_specificites <- function(path = "~/Dropbox/2020-2021/Motifs/",
                                      csv_corpus_motifs = "corpus_motifs_grams.csv",
                                      csv_corpus_specificites = "Corpus_spec_freq.csv", 
                                      frequence = 150){
  
  
  ## Importation des librairies : 
  require("tidytext")
  require("tidyverse")
  require("ggplot2")
  require("tidyr")
  require("data.table")
  require("reshape2")
  require("dplyr")
  
  # Chargement des deux corpus :
  
  corpus_spec <- fread(csv_corpus_specificites, encoding = "UTF-8", 
                       header = TRUE, stringsAsFactors = FALSE)
  
  # Suppression colonne index : 
  
  corpus_spec <- corpus_spec[,-c("V1")]
  
  corpus <- fread(csv_corpus_motifs, encoding = "UTF-8", 
                  header = TRUE, stringsAsFactors = FALSE)
  
  # Vérification okazou (pb index) :
  corpus <- corpus[,c("mots", "ngrammot", "motifs", "Oeuvre")]
  
  ## Retrait des cases vides :
  corpus <- corpus[complete.cases(corpus),]
  
  # Réduction du corpus_spec à nombre_motifs : évite de produire des trop grand csv,
  # réduit le temps de génération, inutile d'analyser des motifs à très basse fréquence...
  
  corpus_spec <- corpus_spec %>%
    dplyr::filter(n > frequence)
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  # Retour aux textes :
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  ## Référence : M. Jockers, Text analysis with R for students of literature, 2014.
  
  # Préalable : choix d'un motif pertinent ! Ex : le NC , le NC
  
  
  
  retour_aux_textes <- function(corpus_spec){
    
    context<- as.numeric(readline("Combien de mots de contexte voulez-vous afficher ? Entrez un nombre : \n"))
    longueur_motif <- as.numeric(readline("Quelle longueur a votre motif : \n"))
    #keyword<- (readline("Entrez le motif : \n"))
    hits <- which(corpus$motifs %in% corpus_spec$motifs)
    
    if(length(hits)>0){
      result<-NULL
      for(h in 1:length(hits))
      {
        start<- hits[h]-context
        if(start < 1){ #if(start < 1 && h == 1){
          start<-1}
        
        end<-hits[h]+context+as.numeric(longueur_motif) # La fin du motif contient aussi le motif en lui-même. 
        
        myrow<-cbind(hits[h], paste(corpus$mots[start:(hits[h]-1)], collapse=" "), 
                     paste(corpus$ngrammot[hits[h]], collapse=" "), 
                     paste(corpus$mots[(hits[h]+longueur_motif):end], collapse=" "), 
                     paste(corpus$Oeuvre[hits[h]], collapse = " "),
                     paste(corpus$motifs[hits[h]], collapse = " "))
        result<-rbind(result,myrow)
        
      }
      colnames(result)<-c("id", "contexte_gauche", "motif", "contexte_droit", "Oeuvre", "motifs")
      result <- as_tibble(result)
      result <- inner_join(result, corpus_spec)
      result <- result[order(result$nrel),]
      toprint<-as.numeric((readline("Sauvegarder les résultats en csv, tapez 1 et enter \n, Sauvegarder dans un objet R result_df, tapez 2 \n")))
      if(toprint==1){
        write.csv(result, "Retour_aux_textes_corpus_specificites.csv", fileEncoding = "UTF-8")
      }
      if(toprint==2){
        result_df <<- result
      }
    }
    else {
      print("Votre motif n'a pas été trouvé")
    }
  }
  
  retour_aux_textes(corpus_spec)
  
}

# Fonction de retour aux textes pour un motif spécifique (màj : 15 mai 2021) :

retour_texte_specificites_un_motif <- function(path = "~/Dropbox/2020-2021/Motifs/",
                                               csv_corpus_motifs = "corpus_motifs_grams.csv",
                                               csv_corpus_specificites = "Corpus_spec_freq.csv",
                                               motif_cible = "de le NC de le NC ,"){
  ## Importation des librairies : 
  require("tidytext")
  require("tidyverse")
  require("ggplot2")
  require("tidyr")
  require("data.table")
  require("reshape2")
  library("dplyr")
  
  # Chargement des deux corpus :
  
  corpus_spec <- fread(csv_corpus_specificites, encoding = "UTF-8", 
                       header = TRUE, stringsAsFactors = FALSE)
  
  # Suppression colonne index : 
  
  corpus_spec <- corpus_spec[,-c("V1")]
  
  corpus <- fread(csv_corpus_motifs, encoding = "UTF-8", 
                  header = TRUE, stringsAsFactors = FALSE)
  
  # Vérification okazou (pb index) :
  corpus <- corpus[,c("mots", "ngrammot", "motifs", "Oeuvre")]
  
  ## Retrait des cases vides :
  corpus <- corpus[complete.cases(corpus),]
  
  
  corpus <- corpus[,c("mots", "ngrammot", "motifs", "Oeuvre")]
  
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  # Retour aux textes :
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  ## Référence : M. Jockers, Text analysis with R for students of literature, 2014.
  
  # Préalable : choix d'un motif pertinent ! Ex : le NC , le NC
  retour_aux_textes <- function(corpus){
    
    context<- as.numeric(readline("Combien de mots de contexte voulez-vous afficher ? Entrez un nombre : \n"))
    longueur_motif <- as.numeric(readline("Quelle longueur a votre motif : \n"))
    keyword<- as.character(motif_cible)
    hits <- which(corpus$motifs == as.character(keyword))
    
    if(length(hits)>0){
      result<-NULL
      for(h in 1:length(hits))
      {
        start<- hits[h]-context
        if(start < 1){ #if(start < 1 && h == 1){
          start<-1}
        
        end<-hits[h]+context+as.numeric(longueur_motif) # La fin du motif contient aussi le motif en lui-même. 
        
        myrow<-cbind(hits[h], paste(corpus$mots[start:(hits[h]-1)], collapse=" "), 
                     paste(corpus$ngrammot[hits[h]], collapse=" "), 
                     paste(corpus$mots[(hits[h]+longueur_motif):end], collapse=" "), 
                     paste(corpus$Oeuvre[hits[h]], collapse = " "),
                     paste(corpus$motifs[hits[h]], collapse = " "))
        result<-rbind(result,myrow)
        
      }
      colnames(result)<-c("id", "contexte_gauche", "motif", "contexte_droit", "Oeuvre", "motifs")
      result <- as_tibble(result)
      result <- inner_join(result, corpus_spec)
      result <- result[order(result$nrel),]
      toprint<-as.numeric((readline("Sauvegarder les résultats en csv, tapez 1 et enter \n, affichez dans le terminal tapez 2 et enter\n Sauvegarder dans un objet R result_df, tapez 3 \n")))
      if(toprint==1){
        write.csv(result, paste(keyword,"_In_", context, ".csv"), fileEncoding = "UTF-8")
      }
      if(toprint==2){
        return(result)
      }
      if(toprint==3){
        result_df <<- as_tibble(result)
      }
    } 
    else {
      print("Votre motif n'a pas été trouvé")
    }
  }
  
  retour_aux_textes(corpus = corpus)
  
}

