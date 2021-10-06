#' Titre : Scripts motifs - Calcul de spécificités
#' Auteurs : Dominique Legallois, Antoine de Sacy
#' Date: 6 octobre 2021.

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

# Entrée : corpus de motifs avec : mots || motifs || Oeuvre

path = "~/Dropbox/2020-2021/Motifs/"
csv = "corpus_motifs_grams.csv"

calcul_de_specificites <- function(path = "~/Dropbox/2020-2021/Motifs/",
                                   csv = "corpus_motifs_grams.csv"){
  
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

calcul_de_specificites(path = "~/Desktop/Motifs/",
                       csv = "corpus_motifs_grams.csv")

# Sortie : Corpus_motifs_specificites.csv avec : Oeuvre || motifs || Spécificités par oeuvres.
# Sortie : Corpus_spec_freq.csv avec : Oeuvre || motifs || n (fréq abs) || total (nb de mots dans l'oeuvre) || nrel (fréq relative) || Spécificités par oeuvres.
