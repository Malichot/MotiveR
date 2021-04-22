# Script pour Dominique Legallois #
# Fonction TF-IDF avec ponctuation #

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

# Entrée : un corpus sous format : mots || motifs || Oeuvre

## Paramètres : 

# path = "~/Dropbox/2019-2020/Stage/Corpus_Retour_au_texte/"
# csv = "Corpus_motifs.csv"==> Sortie du script regex.
# nb_grams: choix du nombre de ngrams.
# nombre_motifs = 20 ==> sélection du nombre de motifs à afficher dans la visualisation.

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

tf_idf_motifs <- function(path = "~/Dropbox/2020-2021/Corpus-test-motifs/",
                          csv = "Corpus_motifs_UDPipe.csv", nb_grams = 5, nombre_motifs = 20){
  
  ## Importation des librairies : 
  
  require("tidytext")
  require("tidyverse")
  require("dplyr")
  require("slider")
  require("ggplot2")
  require("tidyr")
  require("data.table")
  
  # Lecture des données :
  
  setwd(path)
  corpus_spec <- fread(csv, encoding = "UTF-8")
  
  ## Retrait des cases vides :
  
  corpus_spec <- corpus_spec[complete.cases(corpus_spec),]
  
  ## Mise sous la forme tidy :
  
  # Vérification okazou :
  names(corpus_spec) <- c("mots", "motifs", "Oeuvre")
  
  ## Retrait des cases vides :
  corpus_spec <- corpus_spec[complete.cases(corpus_spec),]
  
  ## Fivegrams :
  # corpus_spec_punct <- corpus_spec  %>%
  #   mutate(next_word = lead(motifs),
  #          next_word2 = lead(motifs, 2),
  #          next_word3 = lead(motifs, 3),
  #          next_word4 = lead(motifs, 4)) %>%
  #   filter(!is.na(next_word), !is.na(next_word2), !is.na(next_word3), !is.na(next_word4)) %>%
  #   mutate(ngrammotif = paste(motifs, next_word, next_word2, next_word3, next_word4))
  
  # Nouvelle fonction n-grams pour choix du gram :
  
  # Creating 5-grams means setting .after to 4 and removing last 4 rows
  # library : slider
  corpus_spec_punct <- corpus_spec %>%
    mutate(ngrammotif = slide_chr(motifs, paste, collapse = " ", .after = nb_grams-1)) %>%
    head(-nb_grams)
  
  # Sélection des colonnes motifs ngram et Oeuvre :
  corpus_spec_punct <- corpus_spec_punct[,c("ngrammotif", "Oeuvre")]
  
  names(corpus_spec_punct) <- c("motifs", "Oeuvre")
  
  ## Dénombrement + filtrage éventuel des données : ex : n > 10
  corpus_spec_punct <- corpus_spec_punct %>%
    count(Oeuvre, motifs, sort = TRUE) %>%
    filter(n > 1)
  
  corpus_words_ngrams <- corpus_spec_punct
  
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
    labs(x = NULL, y = "tf-idf") +
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

tf_idf_motifs(path = "~/Dropbox/2020-2021/Corpus-test-motifs/" , csv = "Corpus_motifs_UDPipe.csv",
              nb_grams = 4, nombre_motifs = 20)

