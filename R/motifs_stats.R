#' Statistiques générales
#'
#' Fonction génération de statistiques générales
#'
#' @param corpus_grams data.frame corpus_motifs motifs pour chaque corpus mots | motifs | Oeuvre
#'
#' @param corpus_path string Chemin du csv contenant les corpus_motifs motifs pour chaque corpus
#'
#' @param save_output boolean: Sauvegarde les résultats
#'
#' @param save_path string: Chemin du fichier de sauvergarde
#'
#' @param overwrite boolean: Écrase et sauve de nouveaux les résultats
#'
#' @returns DataFrame: Oeuvre | motifs | n (fréq absolue) | nb_total_mots (dans l'oeuvre) | n_rel
#' (fréquence relative) | spécificités oeuvre par oeuvre | pourcentage (présence du motif par
#' rapport au reste du corpus)
#'
#' @examples
#' \donttest{corpus_path <- system.file("extdata", "example_output", package = "MotiveR")
#' motifs_stats <- motifs_stats(corpus_path=corpus_path)}
#'
#' @export
motifs_stats <- function(corpus_grams = NULL,
                         corpus_path = NULL,
                         save_output = FALSE,
                         save_path = NULL,
                         overwrite = FALSE) {
  # For R CMD check "no visible binding for global variable"
  motifs <- Oeuvre <- n <- index <- n_total <- NULL
  
  # Importation des données
  check_object_param(corpus_grams, corpus_path)
  if (is.null(corpus_grams)) {
    corpus_grams = import_table(corpus_path, file_name = "corpus_motifs_grams.csv")
  }
  # Vérification okazou (pb index) :
  corpus_grams <- corpus_grams[, c("mots", "motifs", "Oeuvre")]
  
  ## Retrait des cases vides :
  corpus_grams <- corpus_grams[complete.cases(corpus_grams), ]
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
  
  ## ## ## ## ## ## ## ## ## ## ## ##  BARYCENTRES ## ## ## ## ## ## ## ## ## ## ## ## ## #
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
  
  corpus_punct <- corpus_grams
  
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
  
  corpus_baryc <-
    dplyr::inner_join(corpus_punct_n, corpus_punct_total)
  ## Colonnes de fréquences relative et absolues. ##
  
  # Somme du nombre d'occ * indice / Nombre total d'occurrence de la forme.
  
  corpus_baryc <- corpus_baryc %>%
    dplyr::ungroup() %>%
    dplyr::mutate(barycentre = n * index / n_total)
  
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
  
  corpus_baryc <- corpus_baryc[order(-corpus_baryc$n_total), ]
  
  # Transformation en pourcentage :
  
  corpus_barycentre_pourcentage <- corpus_baryc %>%
    dplyr::mutate(pourcentage = n / n_total * 100)
  
  poucentage_arrondi <-
    round_df(corpus_barycentre_pourcentage$pourcentage, 2)
  
  # Changement des valeurs dans la dataframe :
  
  corpus_barycentre_pourcentage$pourcentage <- poucentage_arrondi
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
  
  ## ## ## ## ## ## ## ## ## ## ## # SPÉCIFICITÉS # ## ## ## ## ## ## ## ## ## ## ## ## ##
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
  
  ## Dénombrement + filtrage éventuel des données : ex : n > 10
  
  corpus_grams <- corpus_grams %>%
    dplyr::count(Oeuvre, motifs, sort = TRUE)
  
  ## Ajout d'une colonne total words pour normaliser la fréquence (fréquence relative) :
  
  total_words <- corpus_grams %>%
    dplyr::group_by(Oeuvre) %>%
    dplyr::summarize(total = sum(n))
  
  corpus_grams <- dplyr::left_join(corpus_grams, total_words)
  
  ## Calcul de la fréquence relative :
  
  corpus_grams$rel_freq <- corpus_grams$n / corpus_grams$total
  
  corpus_words_ngrams_spec <- corpus_grams
  
  ## Reshaping the data : colonnes = corpus, lignes = motifs et freq
  corpus_lexical_table <-
    xtabs(n ~ motifs + Oeuvre, corpus_words_ngrams_spec)
  
  ## Ré-ordonnancement :
  corpus_lexical_table <-
    corpus_lexical_table[order(-corpus_lexical_table[, 1], corpus_lexical_table[, 1]), ]
  
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
    function(lexicaltable,
             types = NULL,
             parts = NULL) {
      spe <- specificites.probabilities(lexicaltable, types, parts)
      
      #dim(spe);
      spelog <- matrix(0, nrow = nrow(spe), ncol = ncol(spe))
      
      spelog[spe < 0.5] <- log10(spe[spe < 0.5])
      
      spelog[spe > 0.5] <- abs(log10(1 - spe[spe > 0.5]))
      
      spelog[spe == 0.5] <- 0
      
      spelog[is.infinite(spe)] <- 0
      
      spelog <- round(spelog, digits = 4)
      
      rownames(spelog) <- rownames(spe)
      
      colnames(spelog) <- colnames(spe)
      
      class(spelog) <- "specificites"
      
      attr(spelog, "frequency.table") <- lexicaltable
      
      attr(spelog, "types") <- types
      
      attr(spelog, "parts") <- parts
      
      attr(spelog, "corpussize") <- attr(spe, "F")
      
      
      return(spelog)
      
    }
  
  # lexicaltable = a matrix of nrow parts and ncol type
  `specificites.probabilities` <-
    function(lexicaltable,
             types = NULL,
             parts = NULL) {
      #if (!is.numeric(lexicaltable)) stop("The lexical table must contain numeric values.");
      
      colMargin <-
        colSums(lexicaltable)
      # or "F" (the total frequency of all the types).
      rowMargin <-
        rowSums(lexicaltable)
      # or "T" (the size of the parts).
      F <-
        sum(colMargin)
      # The grand total (number of tokens in the corpus).
      
      if (!is.null(types)) {
        # Filter on tokens to be considered.
        if (is.character(types)) {
          # convert the name of types given with "types" into row index numbers.
          if (is.null(rownames(lexicaltable))) {
            stop(
              "The lexical table has no row names and the \"types\" argument is a character vector."
            )
            
          }
          if (!all(types %in% rownames(lexicaltable)))
            stop(paste(
              "Some requested types are not known in the lexical table: ",
              paste(types[!(types %in% rownames(lexicaltable))], collapse =
                      " ")
            ))
          
        } else {
          if (any(types < 1))
            stop("The row index must be greater than 0.")
          
          if (max(types) > nrow(lexicaltable))
            stop("Row index must be smaller than the number of rows.")
          
        }
        lexicaltable <- lexicaltable[, types, drop = FALSE]
        
        colMargin <- colMargin[types]
        
      }
      
      if (!is.null(parts)) {
        # Filter on parts to be considered.
        if (is.character(parts)) {
          # convert the name of parts given with "parts" into col index numbers.
          if (is.null(colnames(lexicaltable))) {
            stop(
              "The lexical table has no col names and the \"parts\" argument is a character vector."
            )
            
          }
          if (!all(parts %in% colnames(lexicaltable)))
            stop(paste(
              "Some requested parts are not known in the lexical table: ",
              paste(parts[!(parts %in% colnames(lexicaltable))], collapse =
                      " ")
            ))
          
        } else {
          if (max(parts) > ncol(lexicaltable))
            stop("Column index must be smaller than the number of cols.")
          
          if (any(parts < 1))
            stop("The col index must be greater than 0.")
          
        }
        lexicaltable <- lexicaltable[parts, , drop = FALSE]
        
        rowMargin <- rowMargin[parts]
        
      }
      
      if (nrow(lexicaltable) == 0 | ncol(lexicaltable) == 0) {
        stop("The lexical table must contains at least one row and one column.")
        
      }
      
      specif <-
        matrix(0.0,
               nrow = nrow(lexicaltable),
               ncol = ncol(lexicaltable))
      
      
      for (i in 1:nrow(lexicaltable)) {
        # We proceed the whole lexical table by row (i.e. by part).
        
        whiteDrawn <-
          lexicaltable[i, ]
        # The frequencies observed in this part for each type.
        white <-
          colMargin
        # The total frequencies in the corpus for each type.
        black <-
          F - white
        # The total complement frequency in the corpus for each type.
        drawn <-
          rowMargin[i]
        # The total number of occurrences in the part.
        
        independance    <-
          (white * drawn) / F
        # The theoretic frequency of each type.
        specif_negative <-
          whiteDrawn <  independance
        # index of observed frequencies below the theoretic frequencies.
        specif_positive <-
          whiteDrawn >= independance
        # index of observed frequencies above the theoretic frequencies.
        
        specif[i, specif_negative] <-
          phyper (whiteDrawn[specif_negative], white[specif_negative], black[specif_negative], drawn)
        
        
        specif[i, specif_positive] <-
          phyper (whiteDrawn[specif_positive] - 1, white[specif_positive], black[specif_positive], drawn)
        
      }
      
      dimnames(specif) <- dimnames(lexicaltable)
      
      
      attr(specif, "F") <- F
      
      return(specif)
      
    }
  
  calcul_spec <- specificites(corpus_clean)
  
  calcul_spec <- as.data.frame.matrix(calcul_spec)
  
  # Transformation des lignes dans la variable motifs :
  
  calcul_spec <-
    data.table::setDT(calcul_spec, keep.rownames = "motifs")[]
  
  ## Ajout de la table de fréquences :
  
  colnames(corpus_words_ngrams_spec) <-
    c("Oeuvre", "motifs", "n", "nb_total_mots", "n_rel")
  
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
  
  ## ## ## ## ## ## ## ## ## ## FUSION DES DATAFRAMES ## ## ## ## ## ## ## ## ## ## ## ## #
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
  
  
  # Fusion calcul de spécificités avec fréquences absolues, nombre total de mots dans le corpus, frequences relatives, specificité.
  
  calcul_spec_freq <-
    dplyr::inner_join(corpus_words_ngrams_spec, calcul_spec)
  
  # Ajout des barycentres :
  
  df_stats <-
    dplyr::inner_join(calcul_spec_freq, corpus_barycentre_pourcentage)
  
  df_stats <- subset(df_stats, select = -c(index))
  
  # Exportation csv :
  if (!is.null(save_path) | save_output) {
    save_data_to_csv(
      df_stats,
      "motifs_stats.csv",
      save_path,
      fileEncoding = "UTF-8",
      overwrite = overwrite
    )
  }
  
  return(df_stats)
  
}
