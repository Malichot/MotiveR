#' Retour au texte d'un motif
#'
#' Fonction pour retour aux textes à partir d'un motif de la table de spécificités
#'
#' @param motif_cible string motif cible
#'
#' @param n_grams int n-grams
#'
#' @param len_context int nombre de mots du contexte à afficher
#'
#' @param corpus_grams data.frame corpus_motifs motifs pour chaque corpus mots | motifs | Oeuvre
#'
#' @param corpus_path string Chemin du csv contenant les corpus_motifs motifs pour chaque corpus
#'
#' @param corpus_spec data.frame corpus specificités
#'
#' @param corpus_spec_path string Chemin du csv contenant les specificités du corpus
#'
#' @param save_output boolean: Sauvegarde les résultats
#'
#' @param save_path string: Chemin du fichier de sauvergarde
#'
#' @param overwrite boolean: Écrase et sauve de nouveaux les résultats
#'
#' @return DataFrame: Oeuvre | motifs | n (fréq absolue) | nb_total_mots (dans l'oeuvre) |
#' n_rel (fréquence relative) | spécificités oeuvre par oeuvre | pourcentage (présence du motif par rapport au reste du corpus)
#'
#' @example
#' corpus_annote <- retour_texte_specificites_un_motif(", à le NC", 4, 4)
#'
#' @export
retour_texte_specificites_un_motif <- function(motif_cible,
                                               len_context,
                                               n_grams,
                                               corpus_grams = NULL,
                                               corpus_path = NULL,
                                               corpus_spec = NULL,
                                               corpus_spec_path = NULL,
                                               save_output = FALSE,
                                               save_path = NULL,
                                               overwrite = FALSE){
  # Chargement des deux corpus :
  check_object_param(corpus_grams, corpus_path)
  check_object_param(corpus_spec, corpus_spec_path)
  if (is.null(corpus_grams)) {
    corpus_grams = import_table(corpus_path, file_name = "corpus_motifs_grams.csv")
  }
  # Vérification okazou (pb index) :
  corpus_grams <-
    corpus_grams[, c("mots", "ngrammot", "motifs", "Oeuvre")]
  ## Retrait des cases vides :
  corpus_grams <- corpus_grams[complete.cases(corpus_grams),]
  corpus_grams <- corpus_grams[,c("mots", "ngrammot", "motifs", "Oeuvre")]
  
  if (is.null(corpus_spec)) {
    corpus_spec = import_table(corpus_spec_path, file_name = "corpus_motifs_spec_freq.csv")
  }
  # Suppression colonne index
  corpus_spec[, V1 := NULL]  
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  # Retour aux textes :
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  ## Référence : M. Jockers, Text analysis with R for students of literature, 2014.
  
  # Préalable : choix d'un motif pertinent ! Ex : le NC , le NC
  retour_aux_textes <- function(corpus_grams){
    keyword<- as.character(motif_cible)
    hits <- which(corpus_grams$motifs == as.character(keyword))
    
    if(length(hits)>0){
      result<-NULL
      for(h in 1:length(hits))
      {
        start<- hits[h]-len_context
        if(start < 1){ #if(start < 1 && h == 1){
          start<-1}
        
        end<-hits[h]+len_context+as.numeric(n_grams) # La fin du motif contient aussi le motif en lui-même. 
        
        myrow<-cbind(hits[h], paste(corpus_grams$mots[start:(hits[h]-1)], collapse=" "), 
                     paste(corpus_grams$ngrammot[hits[h]], collapse=" "), 
                     paste(corpus_grams$mots[(hits[h]+n_grams):end], collapse=" "), 
                     paste(corpus_grams$Oeuvre[hits[h]], collapse = " "),
                     paste(corpus_grams$motifs[hits[h]], collapse = " "))
        result<-rbind(result,myrow)
        
      }
      colnames(result)<-c("id", "contexte_gauche", "motif", "contexte_droit", "Oeuvre", "motifs")
      result <- data.table::as.data.table(result)
      result <- dplyr::inner_join(result, corpus_spec)
      result <- result[order(result$nrel),]

      # Exportation csv :
      if (!is.null(save_path) | save_output) {
        save_data_to_csv(
          result,
          paste0(keyword,"_In_", len_context, ".csv"),
          save_path,
          fileEncoding = "UTF-8",
          overwrite = overwrite
        )
      }
      
      return(result)
    } 
    else {
      message("Votre motif n'a pas été trouvé")
    }
  }
  
  return(retour_aux_textes(corpus_grams = corpus_grams))
  
}