## Stage - Dominique Legallois ##
## Script fonction de retour aux textes sur la table de spécificités ##

# Entrée :

# csv_corpus_motifs = sortie script Regex_CorpusEntier_Motifs_Fonction.R
# csv_corpus_specificites = sortie script Calcul_Specificite.Fonction.R

retour_texte_specificites <- function(csv_corpus_motifs = "~/Dropbox/2019-2020/Stage/Corpus/Corpus_motifs_UDPipe.csv",
                                      csv_corpus_specificites = "~/Dropbox/2019-2020/Stage/Corpus/Corpus_motifs_specificites.csv", 
                                      frequence = 10){
  
  
  ## Importation des librairies : 
  require("tidytext")
  require("tidyverse")
  require("ggplot2")
  require("tidyr")
  require("data.table")
  require("reshape2")
  library("dplyr")
  
  corpus_spec <- fread(csv_corpus_specificites)
  corpus <- fread(csv_corpus_motifs)
  
  ## Fivegrams de motifs :
  
  corpus_five <- corpus  %>%
    mutate(next_word = lead(motifs),
           next_word2 = lead(motifs, 2),
           next_word3 = lead(motifs, 3),
           next_word4 = lead(motifs, 4)) %>%
    filter(!is.na(next_word), !is.na(next_word2)) %>%
    mutate(ngrammotifs = paste(motifs, next_word, next_word2, next_word3, next_word4))
  
  ## Fivegrams texte : 
  
  corpus_five <- corpus_five %>%
    group_by(Oeuvre) %>%
    mutate(next_word = lead(mots),
           next_word2 = lead(mots, 2),
           next_word3 = lead(mots, 3),
           next_word4 = lead(mots, 4)) %>%
    filter(!is.na(next_word), !is.na(next_word2)) %>%
    mutate(ngrammots = paste(mots, next_word, next_word2, next_word3, next_word4))
  
  corpus_five <- corpus_five[,c("mots", "ngrammots", "ngrammotifs", "Oeuvre")]
  
  # Réduction du corpus_spec à nombre_motifs : évite de produire des trop grand csv,
  # réduit le temps de génération, inutile d'analyser des motifs à très basse fréquence...
  
  corpus_spec <- corpus_spec %>%
    dplyr::filter(n > frequence)
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  # Retour aux textes :
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  ## Référence : M. Jockers, Text analysis with R for students of literature, 2014.
  
  # Préalable : choix d'un motif pertinent ! Ex : le NC , le NC
  
  retour_aux_textes <- function(corpus_five){
    
    context<- as.numeric(readline("Combien de mots de contexte voulez-vous afficher ? Entrez un nombre : \n"))
    longueur_motif <- as.numeric(readline("Quelle longueur a votre motif : \n"))
    #keyword<- (readline("Entrez le motif : \n"))
    hits <- which(corpus_five$ngrammotifs %in% corpus_spec$motifs)
    
    if(length(hits)>0){
      result<-NULL
      for(h in 1:length(hits))
      {
        start<- hits[h]-context
        if(start < 1){ #if(start < 1 && h == 1){
          start<-1}
        
        end<-hits[h]+context+5 # La fin du motif contient aussi le motif en lui-même. 
        
        myrow<-cbind(hits[h], paste(corpus_five$mots[start:(hits[h]-1)], collapse=" "), 
                     paste(corpus_five$ngrammots[hits[h]], collapse=" "), 
                     paste(corpus_five$mots[(hits[h]+longueur_motif):end], collapse=" "), 
                     paste(corpus_five$Oeuvre[hits[h]], collapse = " "),
                     paste(corpus_five$ngrammotifs[hits[h]], collapse = " "))
        result<-rbind(result,myrow)
        
      }
      colnames(result)<-c("id", "contexte_gauche", "motif", "contexte_droit", "Oeuvre", "motifs")
      result <- as_tibble(result)
      result <- inner_join(result, corpus_spec)
      result <- result[order(result$nrel),]
      toprint<-as.numeric((readline("Sauvegarder les résultats en csv, tapez 1 et enter \n, Sauvegarder dans un objet R result_df, tapez 2 \n")))
      if(toprint==1){
        write.csv(result, "Retour_aux_textes_corpus_specificites.csv")
      }
      if(toprint==2){
        result_df <<- result
      }
    }
    else {
      print("Votre motif n'a pas été trouvé")
    }
  }
  
  retour_aux_textes(corpus_five)
  
}


retour_texte_specificites(csv_corpus_motifs = "~/Dropbox/2019-2020/Stage/Corpus/Corpus_motifs_UDPipe.csv",
                          csv_corpus_specificites = "~/Dropbox/2019-2020/Stage/Corpus/Corpus_motifs_specificites.csv",
                          frequence = 50)



