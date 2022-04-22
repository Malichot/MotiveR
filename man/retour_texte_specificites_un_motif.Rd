% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/retour_texte_specificites_un_motif.R
\name{retour_texte_specificites_un_motif}
\alias{retour_texte_specificites_un_motif}
\title{Retour au texte d'un motif}
\usage{
retour_texte_specificites_un_motif(
  motif_cible,
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
\item{motif_cible}{string motif cible}

\item{len_context}{int nombre de mots du contexte à afficher}

\item{n_grams}{int n-grams}

\item{corpus_grams}{data.frame corpus_motifs motifs pour chaque corpus mots | motifs | Oeuvre}

\item{corpus_path}{string Chemin du csv contenant les corpus_motifs motifs pour chaque corpus}

\item{corpus_spec}{data.frame corpus specificités}

\item{corpus_spec_path}{string Chemin du csv contenant les specificités du corpus}

\item{save_output}{boolean: Sauvegarde les résultats}

\item{save_path}{string: Chemin du fichier de sauvergarde}

\item{overwrite}{boolean: Écrase et sauve de nouveaux les résultats}
}
\value{
DataFrame: Oeuvre | motifs | n (fréq absolue) | nb_total_mots (dans l'oeuvre) |
n_rel (fréquence relative) | spécificités oeuvre par oeuvre | pourcentage (présence du motif par rapport au reste du corpus)
}
\description{
Fonction pour retour aux textes à partir d'un motif de la table de spécificités
}