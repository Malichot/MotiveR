# Clean env
rm(list = ls(all = TRUE))
graphics.off()
# Detach packages to test if relative imports are valid.
if (!is.null(sessionInfo()$otherPkgs)) {
  lapply(
    paste('package:', names(sessionInfo()$otherPkgs), sep = ""),
    detach,
    character.only = TRUE,
    unload = TRUE
  )
}
# Répertoire de travail :
setwd("/Users/brunospilak/Documents/Perso/Motifs/Motifs")

# Params
library(Motifs)
path = "../Corpus-torun" # chemin du corpus
save_output = TRUE # Sauvegarde résultats
overwrite = TRUE # Écrase résultats précédents
n_grams = 4 # n-gram encodage
frequence = 3
len_context = 4

 
# UDpipe annotation
corpus_annote = annotation_udpipe(path = path,
                                  save_output = save_output,
                                  overwrite = overwrite)
# Étiquetage du corpus
corpus_motifs = regex_corpus_udpipe(corpus = corpus_annote,
                                    save_output = save_output,
                                    overwrite = overwrite)

# Ou pipeline entière
corpus_motifs = tag_motif_pipeline(path = path,
                                   save_output = save_output,
                                   overwrite = overwrite)

# Choix du nombre de ngrams :
corpus_grams =  choix_nb_ngrams(
  n_grams,
  corpus = corpus_motifs,
  save_output = save_output,
  overwrite = overwrite
)

# Wordcloud :
motifs_nuage(corpus_grams = corpus_grams,
             nmots = 10,
             freq = "rel")

# Histogrammes :
motifs_histogram(corpus_grams = corpus_grams,
                 nmots = 10,
                 freq = "rel")

# TF-IDF :
motifs_tf_idf(
  n_motifs = 10,
  plot_type = "group",
  corpus_grams = corpus_grams,
  save_output = FALSE,
  overwrite = TRUE
)

# ACP :
motifs_acp(plot_type = "var", corpus_grams = corpus_grams)
motifs_acp(plot_type = "motif", corpus_grams = corpus_grams)
motifs_acp(plot_type = "var+motif", corpus_grams = corpus_grams)

# Stats
df_stats = motifs_stats(
  corpus_grams = corpus_grams,
  save_output = save_output,
  overwrite = overwrite
)

# Calcul de spécificités
calcul_spec_freq = calcul_specificites(
  save_freq = TRUE,
  retrait_frequence_1 = TRUE,
  corpus_grams = corpus_grams,
  save_output = save_output,
  overwrite = overwrite
)


# Retour aux textes
calcul_spec_freq = retour_texte_specificites(
  frequence = frequence,
  len_context = len_context,
  n_grams = n_grams,
  corpus_grams = corpus_grams,
  corpus_spec = calcul_spec_freq,
  save_output = save_output,
  overwrite = overwrite
)

# Retour aux textes à partir d'un motif
calcul_spec_freq = retour_texte_specificites_un_motif(
  motif_cible = "le NC de le",
  len_context = len_context,
  n_grams = n_grams,
  corpus_grams = corpus_grams,
  corpus_spec = calcul_spec_freq,
  save_output = save_output,
  overwrite = overwrite
)

