#' TF-IDF plot
#'
#' Fonction génération TF-IDF
#'
#' @param corpus_grams data.frame sous format mots || motifs || Oeuvre
#'
#' @param corpus_path string chemin du csv contenant les motifs en ngram
#'
#' @param n_motifs int sélection du nombre de motifs à afficher
#'
#' @param plot_type string "group" pour une visualisation groupée, "sep" pour une visualisation séparée

#' @example
#' motifs_tf_idf(corpus_path="corpus_motifs_grams.csv", n_motifs = 20, plot_type="group")
#'
#' @export
motifs_tf_idf <- function(corpus_grams=NULL,
                          corpus_path=NULL, 
                          n_motifs = 20, 
                          plot_type = "group",
                          save_output = FALSE,
                          overwrite=FALSE){
  # Lecture des données :
  corpus_grams <- fread(corpus_path, encoding = "UTF-8", 
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
  if(plot_type == "sep"){
    tf_idf_grid <- corpus_words_ngrams %>%
      select(-total) %>%
      arrange(desc(tf_idf)) %>%
      mutate(motifs = factor(motifs, levels = rev(unique(motifs)))) %>% 
      group_by(Oeuvre) %>% 
      top_n(n_motifs) %>% # À moduler suivant les besoins : > x ; == x ; etc.
      ungroup %>%
      ggplot(aes(motifs, tf_idf, fill = Oeuvre)) +
      geom_col(show.legend = T) +
      labs(x = NULL, y = "TF-IDF") +
      facet_wrap(~Oeuvre, ncol = 2, scales = "free") +
      coord_flip() +
      theme_minimal()
    return(tf_idf_grid)
  } else if(plot_type == "group"){
    tf_idf_all <- corpus_words_ngrams %>% 
      select(-total) %>%
      arrange(desc(tf_idf)) %>%
      mutate(motifs = factor(motifs, levels = rev(unique(motifs)))) %>% 
      top_n(n_motifs) %>% # À moduler suivant les besoins : > x ; == x ; etc.
      ungroup() %>%
      mutate(word = reorder(motifs, tf_idf)) %>%
      ggplot(aes(motifs, tf_idf, fill = Oeuvre)) +
      geom_bar(stat = "identity") +
      ylab("TF-IDF") +
      coord_flip() +
      theme_minimal()
    return(tf_idf_all)
  } else{
    stop("L'argument plot_type=", plot_type, " n'est pas valide...")
  }
  
  if (save_output) {
    save_path = file.path(OUTPUT_DIR, "tf-idf.csv")
    message("Sauvegarde des motifs dans ", save_path)
    if (!file.exists(save_path)) {
      write.csv(tf_idf_export, save_path)
    } else {
      if (overwrite) {
        warning("Le fichier ", save_path, " existe dèjà, écrase et sauve nouveau. Pour éviter ce comportement, utiliser overwrite = FALSE.")
        write.csv(tf_idf_export, save_path)
      } else {
        stop("Le fichier ", save_path, " existe dèjà. Veuillez le renommer ou le supprimer ou utilisez overwrite=TRUE.")
      }
    }
  }
}