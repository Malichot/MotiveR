#' Titre : Scripts motifs - TF-IDF
#' Auteurs : Dominique Legallois, Antoine de Sacy
#' Date: 6 octobre 2021.

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

# Entrée : un corpus sous format : mots || motifs || Oeuvre

## Paramètres : 

# path = "~/Dropbox/2019-2020/Stage/Corpus_Retour_au_texte/"
# csv = "corpus_motifs_ngrams.csv"==> Sortie du script ngrams
# nombre_motifs = 20 ==> sélection du nombre de motifs à afficher dans la visualisation.

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

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

tf_idf_motifs(path = "~/Desktop/Motifs/",
              csv = "corpus_motifs_grams.csv", nombre_motifs = 20)
