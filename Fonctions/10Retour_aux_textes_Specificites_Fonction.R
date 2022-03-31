#' Titre : Scripts motifs - Fonction pour retour aux textes à partir de la table de spécificités
#' Auteurs : Dominique Legallois, Antoine de Sacy
#' Date: 6 octobre 2021.

# Entrée :

# csv_corpus_motifs = sortie script choix ngrams
# csv_corpus_specificites = sortie script de calcul des spécificités
# frequence = filtre de seuil de fréquence 

path = "~/Dropbox/2020-2021/Motifs/"
csv_corpus_motifs = "corpus_motifs_grams.csv"
csv_corpus_specificites = "Corpus_spec_freq.csv" 
frequence = 25

retour_texte_specificites <- function(path = "~/Dropbox/2020-2021/Motifs/",
                                      csv_corpus_motifs = "corpus_motifs_grams.csv",
                                      csv_corpus_specificites = "Corpus_spec_freq.csv", 
                                      frequence = 150){
  
  
  ## Importation des librairies : 
  require("tidytext")
  require("tidyverse")
  require("tidyr")
  require("data.table")
  require("reshape2")
  require("dplyr")
  require("DT")
  
  # Chargement des deux corpus :
  
  corpus_spec <- fread(csv_corpus_specificites, encoding = "UTF-8", 
                       header = TRUE, stringsAsFactors = FALSE)
  
  # Suppression colonne index : 
  
  corpus_spec <- corpus_spec[,-c("V1")]
  
  corpus <- fread(csv_corpus_motifs, encoding = "UTF-8", 
                  header = TRUE, stringsAsFactors = FALSE)
  
  # Vérification okazou (pb index) :
  corpus <- corpus[,c("mots", "ngrammot", "motifs", "Oeuvre")]
  
  ## Retrait des cases vides :
  corpus <- corpus[complete.cases(corpus),]
  
  # Réduction du corpus_spec à nombre_motifs : évite de produire des trop grand csv,
  # réduit le temps de génération, inutile d'analyser des motifs à très basse fréquence...
  
  corpus_spec <- corpus_spec %>%
    dplyr::filter(n > as.numeric(frequence))
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  # Retour aux textes :
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  ## Référence : M. Jockers, Text analysis with R for students of literature, 2014.
  
  # Préalable : choix d'un motif pertinent ! Ex : le NC , le NC
  
  retour_aux_textes <- function(corpus_spec){
    
    context<- as.numeric(readline("Combien de mots de contexte voulez-vous afficher ? Entrez un nombre : \n"))
    longueur_motif <- as.numeric(readline("Quelle longueur a votre motif : \n"))
    #keyword<- (readline("Entrez le motif : \n"))
    hits <- which(corpus$motifs %in% corpus_spec$motifs)
    
    if(length(hits)>0){
      result<-NULL
      for(h in 1:length(hits))
      {
        start<- hits[h]-context
        if(start < 1){ #if(start < 1 && h == 1){
          start<-1}
        
        end<-hits[h]+context+as.numeric(longueur_motif) # La fin du motif contient aussi le motif en lui-même. 
        
        myrow<-cbind(hits[h], paste(corpus$mots[start:(hits[h]-1)], collapse=" "), 
                     paste(corpus$ngrammot[hits[h]], collapse=" "), 
                     paste(corpus$mots[(hits[h]+longueur_motif):end], collapse=" "), 
                     paste(corpus$Oeuvre[hits[h]], collapse = " "),
                     paste(corpus$motifs[hits[h]], collapse = " "))
        result<-rbind(result,myrow)
        
      }
      colnames(result)<-c("id", "contexte_gauche", "motif", "contexte_droit", "Oeuvre", "motifs")
      result <- as_tibble(result)
      result <- inner_join(result, corpus_spec)
      result <- result[order(result$nrel),]
      toprint<-as.numeric((readline("Sauvegarder les résultats en csv, tapez 1 et enter \n, Sauvegarder dans un objet R result_df, tapez 2 \n, Explorer les résultats dans une table interactive, tapez 3 \n")))
      if(toprint==1){
        write.csv(result, "Retour_aux_textes_corpus_specificites.csv", fileEncoding = "UTF-8")
      }
      if(toprint==2){
        result_df <<- result
      }
      if(toprint==3){
        datatable(data = result, class = "cell-border stripe", options = list(searchHighlight = TRUE))
      }
    }
    else {
      print("Votre motif n'a pas été trouvé")
    }
  }
  
  retour_aux_textes(corpus_spec)
  
}

retour_texte_specificites(path = "~/Documents/Huma-num/2021-2022/Motifs/",
                          csv_corpus_motifs = "corpus_motifs_grams.csv",
                          csv_corpus_specificites = "Corpus_spec_freq.csv",
                          frequence = 170)

