#' Pipeline Motifs UDPipe
#'
#' Étiquetage du corpus et transformation en motifs UDPpipe
#'
#' @param path string: Chemin du dossier contenant les différents corpus.
#'
#' @param corpus string: Chemin du dossier contenant les différents corpus.
#'
#' @param save_output boolean: Sauvegarde les résultats
#'
#' @param overwrite boolean: Écrase et sauve de nouveaux les résultats
#'
#'
#' @return DataFrame: corpus_annote avec les columns (mots || lemmes || POS || feats || Oeuvre)
#'
#' @example
#' corpus_annote <- annotation_udpipe("curpus-test")
#'
#' @export


source("./Fonctions/regex_corpus_udpipe.R")
source("./Fonctions/annotation_udpipe.R")
tag_motif_pipeline <-
  function(path,
           corpus = NULL,
           save_output = TRUE,
           overwrite = FALSE) {
    corpus_annote = annotation_udpipe(path = "./Corpus-torun",
                                      save_output = save_output,
                                      overwrite = overwrite)
    corpus_motifs = regex_corpus_udpipe(corpus = corpus_annote,
                                        save_output = save_output,
                                        overwrite = overwrite)
    
    return (corpus_motifs)
  }
