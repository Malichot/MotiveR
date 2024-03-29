% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/retour_texte_specificites.R
\name{retour_texte_specificites}
\alias{retour_texte_specificites}
\title{Retour au texte depuis les specificites}
\usage{
retour_texte_specificites(
  frequence,
  len_context,
  n_grams,
  corpus_grams = NULL,
  corpus_path = NULL,
  corpus_spec = NULL,
  corpus_spec_path = NULL,
  save_output = FALSE,
  save_path = NULL,
  overwrite = FALSE
)
}
\arguments{
\item{frequence}{int filtre de seuil de frequence}

\item{len_context}{int nombre de mots du contexte a afficher}

\item{n_grams}{int n-grams}

\item{corpus_grams}{data.frame corpus_motifs motifs pour chaque corpus mots | motifs | Oeuvre}

\item{corpus_path}{string Chemin du csv contenant les corpus_motifs motifs pour chaque corpus}

\item{corpus_spec}{data.frame corpus specificites}

\item{corpus_spec_path}{string Chemin du csv contenant les specificites du corpus}

\item{save_output}{boolean: Sauvegarde les resultats}

\item{save_path}{string: Chemin du fichier de sauvergarde}

\item{overwrite}{boolean: Ecrase et sauve de nouveaux les resultats}
}
\value{
DataFrame avec colonnes: Oeuvre | motifs | n (freq absolue) | nb_total_mots (dans
l'oeuvre) |
n_rel (frequence relative) | specificites oeuvre par oeuvre | pourcentage (presence du motif par
rapport au reste du corpus)
}
\description{
Fonction pour retour aux textes a partir de la table de specificites
}
\examples{
file_path <- system.file("extdata", "example_output", "corpus_motifs_grams.csv",
package="MotiveR")
corpus_grams <- data.table::fread(file_path, encoding = "UTF-8", header = TRUE)

file_path <- system.file("extdata", "example_output", "corpus_motifs_spec_freq.csv",
package="MotiveR")
corpus_spec <- data.table::fread(file_path, encoding = "UTF-8", header = TRUE)

corpus_annote <- retour_texte_specificites(frequence=10, len_context=4, n_grams=4,
corpus_grams=corpus_grams, corpus_spec=corpus_spec)

}
