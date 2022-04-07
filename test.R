rm(list = ls(all = TRUE))
graphics.off()


setwd("/Users/brunospilak/Documents/Perso/Motifs/Motifs/")
# assign("motifsEnv", new.env())

# TEST = "annotation_udpipe"
# path = "./Corpus-torun"
# save_output = TRUE
# overwrite = TRUE
# DETACH_PACKAGE = TRUE

# TEST = "regex_corpus_udpipe"
# path = "./"
# save_output = TRUE
# overwrite = TRUE
# corpus_path = "./output-Corpus-torun/udpipe_corpus_complet.csv"
# DETACH_PACKAGE = TRUE

# TEST = "tag_motif_pipeline"
# path = "./Corpus-torun"
# save_output = TRUE
# overwrite = TRUE
# DETACH_PACKAGE = TRUE

# TEST = "choix_nb_ngrams"
# n_grams = 4
# corpus = NULL
# corpus_path = "./output/udpipe_corpus_motifs.csv"
# save_output = TRUE
# overwrite = TRUE
# DETACH_PACKAGE = TRUE

# TEST = "motifs_nuage"
# corpus_grams = NULL
# corpus_path = NULL # "./output/corpus_motifs_grams.csv"
# DETACH_PACKAGE = TRUE


# TEST = "motifs_histogram"
# corpus_grams = NULL
# corpus_path = NULL # "./output-Corpus-torun/corpus_motifs_grams.csv"
# DETACH_PACKAGE = TRUE

TEST = "motifs_tf_idf"
corpus_grams = NULL
corpus_path = NULL # "./output-Corpus-torun/corpus_motifs_grams.csv"
save_output = FALSE
overwrite = FALSE
plot_type = "group"
DETACH_PACKAGE = TRUE

if (DETACH_PACKAGE) {
  if (!is.null(sessionInfo()$otherPkgs)) {
    lapply(
      paste('package:', names(sessionInfo()$otherPkgs), sep = ""),
      detach,
      character.only = TRUE,
      unload = TRUE
    )
  }
}

if (TEST == "annotation_udpipe") {
  source("./R/utils.R")
  source("./R/annotation_udpipe.R")
  # Librairies:
  require("dplyr") # need to handle %>% or magrittr ?
  # require("udpipe")
  # require("tidyverse")
  # require("vroom")
  # require("stringr")
  corpus_annote = annotation_udpipe(path = "./Corpus-torun",
                                    save_output = save_output,
                                    overwrite = overwrite)
} else if (TEST == "regex_corpus_udpipe") {
  source("./R/utils.R")
  source("R/regex_corpus_udpipe.R")
  # Librairies :
  require("dplyr")
  # require("stringr")
  # require("readr")
  # require("data.table")
  corpus_motifs = regex_corpus_udpipe(save_output = save_output, overwrite = overwrite)
} else if (TEST == "tag_motif_pipeline") {
  source("./R/utils.R")
  source("R/annotation_udpipe.R")
  source("R/regex_corpus_udpipe.R")
  source("R/tag_motif_pipeline.R")
  require("dplyr") # need to handle %>% or magrittr ?
  # require("udpipe")
  # require("tidyverse")
  # require("vroom")
  # require("stringr")
  # require("readr")
  # require("data.table")
  corpus_motifs = tag_motif_pipeline(path = "./Corpus-torun",
                                     save_output = save_output,
                                     overwrite = overwrite)
} else if (TEST == "choix_nb_ngrams") {
  source("./R/utils.R")
  source("R/choix_nb_ngrams.R")
  require("dplyr") # need to handle %>%
  # require("tidytext")
  # require("tidyverse")
  # require("data.table")
  corpus_token = choix_nb_ngrams(
    n_grams,
    corpus = corpus,
    corpus_path = corpus_path,
    save_output = save_output,
    overwrite = overwrite
  )
} else if (TEST == "motifs_nuage") {
  source("./R/utils.R")
  source("R/motifs_nuage.R")
  require("dplyr")
  # require("tidytext")
  # require("tidyverse")
  # require("ggwordcloud")
  # require("RColorBrewer")
  # require("reshape2")
  # require("ggsci")
  # require("data.table")
  motifs_nuage(
    corpus_grams = corpus_grams,
    corpus_path = corpus_path,
    nmots = 10,
    freq = "rel"
  )
} else if (TEST == "motifs_histogram") {
  source("./R/utils.R")
  source("R/motifs_histogram.R")
  require("dplyr")
  # require("tidytext")
  # require("tidyverse")
  # require("ggwordcloud")
  # require("RColorBrewer")
  # require("reshape2")
  # require("ggsci")
  # require("data.table")
  motifs_histogram(corpus_grams = corpus_grams,
                   corpus_path = corpus_path,
                   nmots = 10)
} else if (TEST == "motifs_tf_idf") {
  source("./R/utils.R")
  source("R/motifs_tf_idf.R")
  require("dplyr")
  # require("tidytext")
  # require("tidyverse")
  # # require("slider")
  # require("ggplot2")
  # require("tidyr")
  # require("data.table")
  motifs_tf_idf(
    n_motifs = 10,
    plot_type = plot_type,
    corpus_grams = corpus_grams,
    corpus_path = corpus_path,
    save_output = save_output,
    overwrite = overwrite
  )
} else {
  stop("Test is not valid: ", TEST)
}
