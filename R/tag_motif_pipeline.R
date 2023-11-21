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
#' @returns DataFrame avec colonnes (mots || lemmes || POS || feats || Oeuvre)
#'
#' @examples
#' path <- system.file("extdata", "corpus-test", package = "MotiveR")
#' corpus_annote <- tag_motif_pipeline(path, save_output=FALSE)
#'
#' @export
tag_motif_pipeline <-
  function(path,
           corpus = NULL,
           save_output = TRUE,
           overwrite = FALSE) {
    corpus_annote = annotation_udpipe(path = path,
                                      save_output = save_output,
                                      overwrite = overwrite)
    corpus_motifs = regex_corpus_udpipe(corpus = corpus_annote,
                                        save_output = save_output,
                                        overwrite = overwrite)
    
    return (corpus_motifs)
  }
