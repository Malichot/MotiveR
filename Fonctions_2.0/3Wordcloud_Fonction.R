## Fonction Wordcloud : ##

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

# Entrée : un corpus sous format : mots || motifs || Oeuvre

## Paramètres : 

# path = chemin, préférablement celui où se trouve le csv.
# csv = "Corpus_motifs.csv"==> Sortie du script regex.
# nmots = 55 ==> sélection du nombre de motifs à afficher dans la visualisation.

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

# == (sortie du script de regex)

path = "~/Dropbox/2020-2021/Corpus-test-motifs/"
csv = "Corpus_motifs_UDPipe.csv"
nmots = 25
nb_grams = 4

motifs_nuage <- function(path = "~/Dropbox/2020-2021/Corpus-test-motifs/", 
                         csv = "Corpus_motifs_UDPipe.csv", nmots = 25, nb_grams = 4){
  
  # Librairies :
  
  require("dplyr")
  require("tidytext")
  require("tidyverse")
  require("ggwordcloud")
  require("RColorBrewer")
  require("reshape2")
  require("ggsci")
  require("slider")
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
  # 

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
    count(Oeuvre, motifs, sort = TRUE)
  
  ## Ajout d'une colonne total words pour normaliser la fréquence (fréquence relative) :
  
  total_words <- corpus_spec_punct %>%
    group_by(Oeuvre) %>%
    summarize(total = sum(n))
  
  corpus_words_ngrams <- left_join(corpus_spec_punct, total_words, by = "Oeuvre") 
  
  ## Calcul de la fréquence relative :
  
  corpus_words_ngrams$rel_freq <- corpus_words_ngrams$n / corpus_words_ngrams$total
  
  # Ordonnancement par fréquences relatives :
  corpus_words_ngrams <- corpus_words_ngrams[order(corpus_words_ngrams$rel_freq, decreasing = T),] 
  
  ## Visualisation sur les fréquences absolues :
  
  plot_abs <- ggplot(
    corpus_spec_punct[1:nmots,], # TOdo : changer 50 par une variable dans la fonction
    aes(
      label = motifs, size = n,
      x = Oeuvre, color = Oeuvre, fill = Oeuvre 
    )
  ) +
    geom_text_wordcloud_area(shape = "diamond") +
    scale_size_area(max_size = 15) +
    scale_x_discrete() +
    theme_minimal()
  
  ## Visualisation sur les fréquences relatives :
  
  plot_freq <- ggplot(
    corpus_words_ngrams[1:nmots,], # Choix du nombre de motifs à faire apparaître
    aes(
      label = motifs, size = rel_freq,
      x = Oeuvre, color = Oeuvre, fill = Oeuvre 
    )
  ) +
    geom_text_wordcloud_area(shape = "diamond") +
    scale_size_area(max_size = 15) + # à moduler suivant le nb de motifs
    scale_x_discrete() +
    theme_minimal()
  
  
  freq_rel <- as.numeric(readline("Fréquences relatives, tapez 1 et enter \n Fréquences absolues, tapez 2 et enter"))
  
  if(freq_rel == 1){
    return(plot_freq)
    
  }
  
  
  if(freq_rel == 2){
    return(plot_abs)
  }
  
  else{
    print("Votre choix ne correspond pas aux critères binaires proposés...!")
  }
  
}

  
motifs_nuage(path = "~/Dropbox/2020-2021/Corpus-test-motifs/", 
             csv = "Corpus_motifs_UDPipe.csv", nmots = 25, nb_grams = 4)


    
