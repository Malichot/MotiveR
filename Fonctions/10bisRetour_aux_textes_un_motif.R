## Titre : Scripts motifs - Fonction pour retour aux textes à partir d'un motif de la table de spécificités
## Auteurs : Dominique Legallois, Antoine de Sacy
## Date : 15 mai 2021.

## Script de retour aux textes 1 motif  ## 

retour_texte_specificites_un_motif <- function(path = "~/Dropbox/2020-2021/Motifs/",
                                               csv_corpus_motifs = "corpus_motifs_grams.csv",
                                               csv_corpus_specificites = "Corpus_spec_freq.csv",
                                               motif_cible = "de le NC de le NC ,"){
  ## Importation des librairies : 
  require("tidytext")
  require("tidyverse")
  require("ggplot2")
  require("tidyr")
  require("data.table")
  require("reshape2")
  library("dplyr")
  
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
  
  
  corpus <- corpus[,c("mots", "ngrammot", "motifs", "Oeuvre")]
  
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  # Retour aux textes :
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  ## Référence : M. Jockers, Text analysis with R for students of literature, 2014.
  
  # Préalable : choix d'un motif pertinent ! Ex : le NC , le NC
  retour_aux_textes <- function(corpus){
    
    context<- as.numeric(readline("Combien de mots de contexte voulez-vous afficher ? Entrez un nombre : \n"))
    longueur_motif <- as.numeric(readline("Quelle longueur a votre motif : \n"))
    keyword<- as.character(motif_cible)
    hits <- which(corpus$motifs == as.character(keyword))
    
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
      toprint<-as.numeric((readline("Sauvegarder les résultats en csv, tapez 1 et enter \n, affichez dans le terminal tapez 2 et enter\n Sauvegarder dans un objet R result_df, tapez 3 \n Affichez les résultats dans une table interactive, tapez 4 \n")))
      if(toprint==1){
        write.csv(result, paste(keyword,"_In_", context, ".csv"), fileEncoding = "UTF-8")
      }
      if(toprint==2){
        return(result)
      }
      if(toprint==3){
        result_df <<- as_tibble(result)
      }
      if(toprint==4){
        datatable(data = result, class = "cell-border stripe", options = list(searchHighlight = TRUE))
      }
    } 
    else {
      print("Votre motif n'a pas été trouvé")
    }
  }
  
  retour_aux_textes(corpus = corpus)
  
}

  
retour_texte_specificites_un_motif(path = "~/Documents/Huma-num/2021-2022/Motifs/",
                                   csv_corpus_motifs = "corpus_motifs_grams.csv",
                                   csv_corpus_specificites = "Corpus_spec_freq.csv",
                                   motif_cible = "de le NC de le")








