## Fonction Wordcloud : ##

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

# Entrée : un corpus sous format : mots || motifs || Oeuvre

## Paramètres : 

# path = chemin, préférablement celui où se trouve le csv.
# csv = "Corpus_motifs.csv"==> Sortie du script regex.
# nmots = 55 ==> sélection du nombre de motifs à afficher dans la visualisation.

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

# == (sortie du script de regex)


motifs_histograms <- function(path = "~/Dropbox/2020-2021/Corpus-test-motifs/", 
                         csv = "Corpus_motifs_UDPipe.csv", nmots = 25, nb_grams = 4){
  
  # Librairies :
  
  require("dplyr")
  require("tidytext")
  require("tidyverse")
  require("RColorBrewer")
  require("reshape2")
  require("ggsci")
  require("slider")
  require("data.table")
  require("ggpubr")
  
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
    mutate(ngrammotif = slide_chr(motifs, paste, collapse = " ", .after = nb_grams-1))
  # head(-nb_grams-1) : ne fonctionne pas : Value of SET_STRING_ELT() must be a 'CHARSXP' not a 'character'
  
  # Transformation en tibble pour éviter l'erreur ?
  
  nb <- nb_grams-1
  corpus_spec_punct <- as_tibble(corpus_spec_punct) %>%
    head(-nb)
  
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
  
  # Visualisation en histogrammes : 
  
  ## Pour visualisation par fréquences décroissantes :
  
  #Turn your 'treatment' column into a character vector
  corpus_words_ngrams$motifs <- as.character(corpus_words_ngrams$motifs)
  #Then turn it back into a factor with the levels in the correct order
  corpus_words_ngrams$motifs <- factor(corpus_words_ngrams$motifs, levels=unique(corpus_words_ngrams$motifs))
  
  ## Fréquences absolues :
  
  plot_abs <- ggplot(data = corpus_words_ngrams[1:nmots,],
                           aes(x = motifs, y = n, fill = Oeuvre)) +
    geom_histogram(stat = "identity", position = "dodge", show.legend = T) + 
    theme_minimal() +
    theme(legend.position="top")
  
  plot_abs <- plot_abs + theme(axis.text.x= element_text(angle = 50, hjust = 1))
  
  ## Fréquences relatives
  
  # Réordonnancement de la df par fréquences relatives :
  
  df_freq_rel <- corpus_words_ngrams %>%
    arrange(-rel_freq)
  
  # Visualisation :
  
  plot_freq <- ggplot(data = df_freq_rel[1:nmots,],
                           aes(x = motifs, y = rel_freq, fill = Oeuvre)) +
    geom_histogram(stat = "identity", position = "dodge", show.legend = T) + 
    theme_minimal() +
    theme(legend.position="top")
  
  plot_freq <- plot_freq + theme(axis.text.x= element_text(angle = 50, hjust = 1))
  
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


motifs_histograms(path = "~/Dropbox/2020-2021/Corpus-test-motifs/", 
             csv = "Corpus_motifs_UDPipe.csv", nmots = 30, nb_grams = 5)

